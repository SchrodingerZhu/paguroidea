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

use std::collections::HashMap;

use syn::{parse_quote, Type};

use super::{semact::SemAct, BoundTarget, NormalForm, Tag};

#[derive(Clone)]
pub enum InferredType {
    Concrete(Type),
    Collector(Box<Self>),
    Option(Box<Self>),
    Tuple(Vec<Self>),
}

pub struct InferenceContext<'a> {
    /// Typed tags
    gamma: HashMap<Tag, InferredType>,
    /// Fully normalized terms
    nforms: &'a HashMap<Tag, Vec<NormalForm>>,
}

impl<'a> InferenceContext<'a> {
    /// Create a new inference context
    pub fn new(
        annotations: &'a HashMap<Tag, Type>,
        nforms: &'a HashMap<Tag, Vec<NormalForm>>,
    ) -> Self {
        let gamma = annotations
            .iter()
            .map(|(k, v)| (k.clone(), InferredType::Concrete(v.clone())))
            .collect();
        Self { gamma, nforms }
    }

    fn infer_gather<'i, I: Iterator<Item = BoundTarget<'i>>>(
        &mut self,
        mut tags: I,
    ) -> Option<InferredType> {
        if let Some(tag) = tags.next() {
            let mut types = vec![if let BoundTarget::Tag(tag) = tag {
                self.infer(tag)?
            } else {
                InferredType::Concrete(parse_quote!(::pag_util::Span<'src>))
            }];
            for t in tags {
                // If any inference fails, the whole inference fails
                let ty = if let BoundTarget::Tag(t) = t {
                    self.infer(t)?
                } else {
                    InferredType::Concrete(parse_quote!(::pag_util::Span<'src>))
                };
                types.push(ty);
            }
            if types.len() == 1 {
                // If there is only one field, no need to wrap in a tuple
                Some(types.pop().unwrap())
            } else {
                // Otherwise, wrap in a tuple
                Some(InferredType::Tuple(types))
            }
        } else {
            // no field, unit type
            Some(InferredType::Concrete(parse_quote!(())))
        }
    }

    /// try infer all types, but may fail with incomplete type information.
    pub fn infer_all_types(mut self) -> HashMap<Tag, InferredType> {
        let mut typed = 0;
        while typed < self.nforms.len() {
            typed = 0;
            for i in self.nforms.keys() {
                if self.infer(i).is_some() {
                    typed += 1;
                }
            }
        }
        self.gamma
    }

    fn infer(&mut self, tag: &Tag) -> Option<InferredType> {
        if let Some(x) = self.gamma.get(tag) {
            return Some(x.clone());
        }

        // Find first subexpression that fulfills inference
        let nfs = self.nforms.get(tag)?;
        let mut inferred = None;
        for i in nfs.iter() {
            let semact = i.semact();
            match semact {
                SemAct::Recognize => {
                    inferred.replace(InferredType::Concrete(parse_quote!(())));
                    break;
                }
                // Token semantic action, the type is Span
                SemAct::Token => {
                    inferred.replace(InferredType::Concrete(parse_quote!(::pag_util::Span<'src>)));
                    break;
                }
                // Customized routine without type annotation, cannot infer
                SemAct::Customized(..) => continue,
                // Nested routine for one or more, the type is unit.
                SemAct::OneOrMoreNested => {
                    inferred.replace(InferredType::Concrete(parse_quote!(())));
                    break;
                }
                SemAct::Gather => {
                    let visible = i.visible_bindings(0);
                    if let Some(gather_type) = self.infer_gather(visible.into_iter().map(|x| x.1)) {
                        inferred.replace(gather_type);
                        break;
                    }
                }
                SemAct::ZeroOrMore | SemAct::Option | SemAct::OneOrMoreToplevel => {
                    let mapper = |ty: InferredType| {
                        if matches!(semact, SemAct::Option) {
                            InferredType::Option(Box::new(ty))
                        } else {
                            InferredType::Collector(Box::new(ty))
                        }
                    };
                    // Skip epsilon production, this is safe since OneOrMoreToplevel will never be empty
                    if let NormalForm::Empty(x, _) = i {
                        if x.is_empty() {
                            continue;
                        }
                    }
                    // skip the trailing part of OneOrMoreToplevel
                    let visible =
                        i.visible_bindings(if matches!(semact, SemAct::OneOrMoreToplevel) {
                            1
                        } else {
                            0
                        });
                    if let Some(gather_type) = self.infer_gather(visible.into_iter().map(|x| x.1)) {
                        inferred.replace(mapper(gather_type));
                        break;
                    }
                }
            }
        }

        let target = inferred?;
        self.gamma.insert(tag.clone(), target.clone());
        Some(target)
    }
}
