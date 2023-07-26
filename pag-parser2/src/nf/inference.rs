// If there is no semantic action, the routine is plain scan over. Thus, the type is unit.
// ⊢ x = ..., SemAct[x] = ∅
// -------------------
// ⊢ x : ()

// A Customized Routine must have type annotation
// ⊢ x = ..., SemAct[x] = Customized(𝜏)
// -------------------
// ⊢ x : 𝜏

// A Token action gives the span of a terminal
// ⊢ x = T, SemAct[x] = Token
// -------------------
// ⊢ x : Span

// Fully normalized Option must be in the following form:
// x = T_0 ... [r_0] | T_1 ... [r_1] | .. | ε
//
// Thus, the rule should be:
//
// Γ ⊢ x = T_0 ... [r_0] | T_1 ... [r_1] | .. | ε
// Γ ⊢ r_0 : 𝜏_0, r_1 : 𝜏_1 ...
// Γ ⊢ 𝜏 = 𝜏_0 = 𝜏_1 = ...
// SemAct[x] = Option
// -------------------
//  Γ ⊢ x : Option<𝜏>

// Fully normalized ZeroOrMore must be in the following form:
// x = T_0 ... [r_0] | T_1 ... [r_1] | .. | ε
//
// Thus, the rule should be:
//
// Γ ⊢ x = T_0 ... [r_0] | T_1 ... [r_1] | .. | ε
// Γ ⊢ r_0 : 𝜏_0, r_1 : 𝜏_1 ...
// Γ ⊢ 𝜏 = 𝜏_0 = 𝜏_1 =...
// SemAct[x] = ZeroOrMore(Σ ∈ Collector<𝜏>)
// -------------------
//  Γ ⊢ x : Σ

// Fully normalized OneOrMoreToplevel must be in the following form:
// x = T_0 ...[r_0] t | T_1 ... [r_1] t | ..
//
// Thus, the rule should be:
//
// Γ ⊢ x = T_0 ...[r_0] t | T_1 ... [r_1] t | ..
// Γ ⊢ r_0 : 𝜏_0, r_1 : 𝜏_1 ...
// Γ ⊢ 𝜏 = 𝜏_0 = 𝜏_1 = ...
// SemAct[x] = OneOrMoreToplevel
// -------------------
//  Γ ⊢ x : Σ

// Fully normalized OneOrMoreNested must be in the following form:
// x = T_0 ... [r_0] | T_1 ... [r_1] | .. | ε
//
// Thus, the rule should be:
//
// Γ ⊢ x = T_0 ... [r_0] | T_1 ... [r_1] | .. | ε
// Γ ⊢ r_0 : 𝜏_0, r_1 : 𝜏_1 ...
// Γ ⊢ 𝜏 = 𝜏_0 = 𝜏_1 =...
// SemAct[x] = ZeroOrMore
// -------------------
//  Γ ⊢ x : () -- Notice that x accept &mut C ∈ Collector<𝜏> instead

// Fully normalized Tuple must be in the following form:
// x = T_0 ... [r_0] x00 _x01 x02 | ..
// let η_i be the type tuple of everything including last reduce that gives an output.
// x = T_0 ... [r_0] x00 _x01 x02 | ..
// Γ ⊢ ║ η_0 ║ = ║ η_1 ║ = ...
// Γ ⊢ ∀i.∀j.∀k. η_i.k = η_j.k
// SemAct[x] = Gather
// -------------------
// Γ ⊢ x : η

use std::{
    cell::UnsafeCell,
    collections::{hash_map::Entry, HashMap},
};

use syn::{parse_quote, Type};

use crate::{frontend::TypeAnnotation};

use super::{
    semact::{SemAct, SemActTable},
    NormalForm, Tag,
};

pub struct InferenceContext<'a> {
    /// Typed tags
    gamma: UnsafeCell<HashMap<Tag, Type>>,
    /// Type annotations from user
    annotations: &'a HashMap<Tag, TypeAnnotation>,
    /// Semantic action table
    semact: &'a SemActTable,
    /// Fully normalized terms
    nforms: &'a HashMap<Tag, Vec<NormalForm>>,
}
impl<'a> InferenceContext<'a> {
    /// Create a new inference context
    pub fn new(
        annotations: &'a HashMap<Tag, TypeAnnotation>,
        semact: &'a SemActTable,
        nforms: &'a HashMap<Tag, Vec<NormalForm>>,
    ) -> Self {
        Self {
            gamma: UnsafeCell::new(HashMap::new()),
            annotations,
            semact,
            nforms,
        }
    }
    fn infer_gather<'i, I: Iterator<Item = &'i Tag>>(&self, mut tags: I) -> Option<Type> {
        if let Some(tag) = tags.next() {
            let mut types = vec![self.infer(tag)?];
            for t in tags {
                // If any inference fails, the whole inference fails
                let ty = self.infer(t)?;
                types.push(ty);
            }
            if types.len() == 1 {
                // If there is only one field, no need to wrap in a tuple
                Some(types.pop().unwrap())
            } else {
                // Otherwise, wrap in a tuple
                Some(parse_quote!((#(#types),*)))
            }
        } else {
            // no field, unit type
            Some(parse_quote!(()))
        }
    }
    fn infer(&self, tag: &Tag) -> Option<Type> {
        match unsafe { (*self.gamma.get()).entry(tag.clone()) } {
            // If a tag has been inferred, return its type directly
            Entry::Occupied(entry) => Some(entry.get().clone()),
            Entry::Vacant(slot) => Some(
                slot.insert({
                    // If a concrete type annotation is provided, use it directly
                    if let Some(x) = self.annotations.get(tag).and_then(|anno| match anno {
                        TypeAnnotation::Concrete(ty) => Some(ty.clone()),
                        _ => None,
                    }) {
                        x
                    } else {
                        let semact = self.semact.get(tag);
                        match semact {
                            // No semantic action, the type is unit
                            None => parse_quote!(()),
                            // Token semantic action, the type is Span
                            Some(SemAct::Token) => parse_quote!(::pag_runtime::Span<'src>),
                            // Customized routine without type annotation -- inference failed
                            Some(SemAct::CustomizedRoutine(..)) => return None,
                            // Nested routine for one or more, the type is unit.
                            Some(SemAct::OneOrMoreNested) => parse_quote!(()),
                            Some(SemAct::Gather) => {
                                let nfs = self.nforms.get(tag)?;
                                let mut inferred = None;
                                // find first subexpression that fulfills inference
                                for i in nfs.iter() {
                                    let visible = i.visible_bindings(0);
                                    if let Some(gather_type) =
                                        self.infer_gather(visible.into_iter().map(|x| x.1))
                                    {
                                        inferred.replace(gather_type);
                                        break;
                                    }
                                }
                                inferred?
                            }
                            Some(SemAct::ZeroOrMore) | Some(SemAct::Option) | Some(SemAct::OneOrMoreToplevel) => {
                                let nfs = self.nforms.get(tag)?;
                                let TypeAnnotation::HigherKind(path) = self
                                    .annotations.get(tag).cloned().unwrap_or_else(||
                                        if matches!(semact, Some(SemAct::Option)) {
                                            TypeAnnotation::HigherKind(parse_quote!(::std::option::Option))
                                        } else {
                                        TypeAnnotation::HigherKind(parse_quote!(::std::collections::VecDeque)) })
                                    else { unreachable!("must be higher kind type") };
                                let mut inferred = None;
                                // find first subexpression that fulfills inference
                                for i in nfs.iter() {
                                    // Skip epsilon production, this is safe since OneOrMoreToplevel will never be empty
                                    if let NormalForm::Empty(x) = i {
                                        if x.is_empty() {
                                            continue;
                                        }
                                    }
                                    // skip the trailing part of OneOrMoreToplevel
                                    let visible = i.visible_bindings(
                                        if matches!(semact, Some(SemAct::OneOrMoreToplevel)) {
                                            1
                                        } else {
                                            0
                                        },
                                    );
                                    if let Some(gather_type) =
                                        self.infer_gather(visible.into_iter().map(|x| x.1))
                                    {
                                        inferred.replace(
                                            parse_quote!(#path<#gather_type>),
                                        );
                                        break;
                                    }
                                }
                                inferred?
                            }
                        }
                    }
                })
                .clone(),
            ),
        }
    }
}
