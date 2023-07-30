//!
//! Transform from surface syntax to semi-normalized form
//!

use std::collections::HashMap;
use std::rc::Rc;

use quote::format_ident;
use syn::{Ident, Type};

use super::{semact::SemAct, Action, NormalForm, Tag};
use super::{AbstractType, NFTable};
use crate::frontend::{Ast, ParserDef, ParserExpr, SequenceIterator};

struct Translation {
    /// Table of semi-normalized production rules
    semi_nfs: NFTable,
    /// Toplevel type annotations
    annotations: HashMap<Tag, Rc<Type>>,
    /// Type hints when calling inner routines (collector)
    hints: HashMap<Tag, Rc<Type>>,
    /// Counter of assigned non-explicit variable names
    output_cnt: usize,
    /// Counter of assigned anonymous routines
    anonymous_cnt: usize,
    /// Whether we are currently ignoring the output
    ignoring_cnt: usize,
}

type NFAttrTuple = (Tag, Option<Ident>, AbstractType);

impl From<&'_ Ast> for Translation {
    fn from(value: &Ast) -> Self {
        let annotations = value
            .parser_map
            .iter()
            .map(|(ident, def)| (Tag::Toplevel(ident.clone()), def.ty.clone()))
            .collect();
        let mut translation = Self {
            semi_nfs: Default::default(),
            annotations,
            hints: Default::default(),
            output_cnt: 0,
            anonymous_cnt: 0,
            ignoring_cnt: 0,
        };
        for (i, def) in &value.parser_map {
            translation.add_toplevel_def(i.clone(), def);
        }
        translation
    }
}

impl Translation {
    /// Enter ignoring mode
    fn start_ignoring(&mut self) {
        self.ignoring_cnt += 1;
    }

    /// Exit ignoring mode
    fn end_ignoring(&mut self) {
        self.ignoring_cnt -= 1;
    }

    fn ignoring(&mut self) -> bool {
        self.ignoring_cnt > 0
    }

    /// Allocate a new symbol for unamed variable bindings.
    fn new_output_sym(&mut self) -> Ident {
        let result = format_ident!("_{}", self.output_cnt);
        self.output_cnt += 1;
        result
    }

    /// Allocate a new tag for anonymous routines.
    fn new_anonymous_tag(&mut self) -> Tag {
        let result = Tag::Anonymous(self.anonymous_cnt);
        self.anonymous_cnt += 1;
        result
    }

    fn infer_type<I: ExactSizeIterator<Item = AbstractType>>(
        &self,
        mut inner_types: I,
        semact: &SemAct,
        tag: &Tag,
    ) -> AbstractType {
        match semact {
            SemAct::Customized(_) => AbstractType::Concrete(self.annotations[tag].clone()),
            SemAct::Gather => match inner_types.len() {
                0 => AbstractType::unit_type(),
                1 => inner_types.next().unwrap(),
                _ => AbstractType::Tuple(inner_types.collect()),
            },
            SemAct::Option => match inner_types.len() {
                0 => AbstractType::Option(Box::new(AbstractType::unit_type())),
                1 => AbstractType::Option(Box::new(inner_types.next().unwrap())),
                _ => AbstractType::Option(Box::new(AbstractType::Tuple(inner_types.collect()))),
            },
            SemAct::ZeroOrMore => match inner_types.len() {
                0 => AbstractType::Collector(Box::new(AbstractType::unit_type())),
                1 => AbstractType::Collector(Box::new(inner_types.next().unwrap())),
                _ => AbstractType::Collector(Box::new(AbstractType::Tuple(inner_types.collect()))),
            },
            SemAct::OneOrMoreToplevel => match inner_types.len() {
                0 => AbstractType::Collector(Box::new(AbstractType::unit_type())),
                1 => AbstractType::Collector(Box::new(inner_types.next().unwrap())),
                _ => AbstractType::Collector(Box::new(AbstractType::Tuple(inner_types.collect()))),
            },
            SemAct::OneOrMoreNested => AbstractType::unit_type(),
            SemAct::Token => AbstractType::span_type(),
            SemAct::Recognize => AbstractType::unit_type(),
        }
    }

    /// Construct a normal form from a sequence of parser expressions.
    fn create_nf_from_sequence<
        'a,
        const IGNORE_UNNAMED: bool,
        I: Iterator<Item = (&'a ParserExpr, Option<Ident>)>,
    >(
        &mut self,
        mut iter: I,
        semact: SemAct,
        tag: &Tag,
    ) -> NormalForm {
        debug_assert_eq!(
            self.ignoring(),
            matches!(semact, SemAct::Recognize),
            "semact must be Recognize in ignoring mode"
        );
        match iter.next() {
            None => NormalForm::Empty {
                actions: vec![],
                semact,
                ty: AbstractType::unit_type().into(),
            },
            Some((ParserExpr::Ignore(box ParserExpr::LexerRef(token)), _)) => {
                let mut types = Vec::new();
                let actions = iter
                    .map(|(inner, named)| self.add_anonymous_rule::<IGNORE_UNNAMED>(inner, named))
                    .map(|(tag, output, ty)| {
                        if output.is_some() {
                            types.push(ty);
                        }
                        Action::Shift { tag, output }
                    })
                    .collect();
                let ty = self.infer_type(types.into_iter(), &semact, tag).into();
                NormalForm::Sequence {
                    token: token.clone(),
                    token_output: None,
                    actions,
                    semact,
                    ty,
                }
            }
            Some((ParserExpr::LexerRef(token), named)) => {
                let mut types = Vec::new();
                if named.is_some() {
                    types.push(AbstractType::span_type())
                }
                let actions = iter
                    .map(|(inner, named)| self.add_anonymous_rule::<IGNORE_UNNAMED>(inner, named))
                    .map(|(tag, output, ty)| {
                        if output.is_some() {
                            types.push(ty);
                        }
                        Action::Shift { tag, output }
                    })
                    .collect();
                let ty = self.infer_type(types.into_iter(), &semact, tag).into();
                NormalForm::Sequence {
                    token: token.clone(),
                    token_output: if matches!(semact, SemAct::Recognize) {
                        None
                    } else if IGNORE_UNNAMED {
                        named
                    } else {
                        named.or_else(|| Some(self.new_output_sym()))
                    },
                    actions,
                    semact,
                    ty,
                }
            }
            Some((expr, named)) => {
                let mut types = Vec::new();
                let actions = [(expr, named)]
                    .into_iter()
                    .chain(iter)
                    .map(|(inner, named)| self.add_anonymous_rule::<IGNORE_UNNAMED>(inner, named))
                    .map(|(tag, output, ty)| {
                        if output.is_some() {
                            types.push(ty);
                        }
                        Action::Shift { tag, output }
                    })
                    .collect();
                let ty = self.infer_type(types.into_iter(), &semact, tag).into();
                NormalForm::Unexpanded {
                    actions,
                    semact,
                    ty,
                }
            }
        }
    }

    fn add_nf(&mut self, tag: Tag, nf: NormalForm) {
        self.semi_nfs.entry(tag).or_default().push(nf);
    }

    fn add_nf_from_anonymous_expr(&mut self, expr: &ParserExpr, tag: &Tag) -> AbstractType {
        match expr {
            ParserExpr::Seq(exprs) => {
                let semact = if self.ignoring() {
                    SemAct::Recognize
                } else {
                    SemAct::Gather
                };
                let partial_nf = self.create_nf_from_sequence::<false, _>(
                    exprs.iter().map(|expr| (expr, None)),
                    semact,
                    tag,
                );
                let ty = partial_nf.ty().0.clone();
                self.add_nf(tag.clone(), partial_nf);
                ty
            }
            ParserExpr::Opt(inner) => {
                let semact = if self.ignoring() {
                    SemAct::Recognize
                } else {
                    SemAct::Option
                };
                let mut partial_nf = self.create_nf_from_sequence::<false, _>(
                    SequenceIterator::from(inner.as_ref()).map(|expr| (expr, None)),
                    semact.clone(),
                    tag,
                );
                let ty = partial_nf.ty().clone();
                *partial_nf.semact_mut() = semact.clone();
                self.add_nf(tag.clone(), partial_nf);
                // add one more rule for empty
                self.add_nf(
                    tag.clone(),
                    NormalForm::Empty {
                        actions: vec![],
                        semact,
                        ty: ty.clone(),
                    },
                );
                ty.0
            }
            ParserExpr::Star(inner) => {
                let semact = if self.ignoring() {
                    SemAct::ZeroOrMore
                } else {
                    SemAct::Option
                };
                let mut partial_nf = self.create_nf_from_sequence::<false, _>(
                    SequenceIterator::from(inner.as_ref()).map(|expr| (expr, None)),
                    semact.clone(),
                    tag,
                );
                let ty = partial_nf.ty().clone();
                *partial_nf.semact_mut() = semact.clone();
                self.add_nf(tag.clone(), partial_nf);
                // add one more rule for empty
                self.add_nf(
                    tag.clone(),
                    NormalForm::Empty {
                        actions: vec![],
                        semact,
                        ty: ty.clone(),
                    },
                );
                ty.0
            }
            ParserExpr::Plus(inner) => {
                let semact = if self.ignoring() {
                    SemAct::Recognize
                } else {
                    SemAct::OneOrMoreToplevel
                };
                let mut partial_nf = self.create_nf_from_sequence::<false, _>(
                    SequenceIterator::from(inner.as_ref()).map(|expr| (expr, None)),
                    semact.clone(),
                    tag,
                );
                let nested_tag = self.new_anonymous_tag();
                // the nested routine
                {
                    let semact = if self.ignoring() {
                        SemAct::Recognize
                    } else {
                        SemAct::OneOrMoreNested
                    };

                    self.add_nf(nested_tag.clone(), {
                        let mut nf = partial_nf.clone();
                        nf.actions_mut().push(Action::TailCall);
                        *nf.semact_mut() = semact.clone();
                        nf
                    });

                    self.add_nf(
                        nested_tag.clone(),
                        NormalForm::Empty {
                            actions: vec![],
                            semact,
                            ty: AbstractType::unit_type().into(),
                        },
                    );
                }
                // the toplevel routine
                {
                    partial_nf
                        .actions_mut()
                        .push(Action::PassCollector(tag.clone()));
                    let ty = partial_nf.ty().0.clone();
                    self.add_nf(tag.clone(), partial_nf);
                    ty
                }
            }
            ParserExpr::LexerRef(ident) => {
                let nf = if self.ignoring() {
                    NormalForm::Sequence {
                        token: ident.clone(),
                        token_output: None,
                        actions: vec![],
                        semact: SemAct::Recognize,
                        ty: AbstractType::unit_type().into(),
                    }
                } else {
                    NormalForm::Sequence {
                        token: ident.clone(),
                        token_output: Some(self.new_output_sym()),
                        actions: vec![],
                        semact: SemAct::Token,
                        ty: AbstractType::span_type().into(),
                    }
                };
                let ty = nf.ty().0.clone();
                self.add_nf(tag.clone(), nf);
                ty
            }
            ParserExpr::ParserRef(_) => unreachable!("cannot create nf from parser ref"),
            ParserExpr::Ignore(_) => unreachable!("cannot create nf from ignore"),
        }
    }

    fn add_anonymous_rule<const IGNORE_UNNAMED: bool>(
        &mut self,
        expr: &ParserExpr,
        named: Option<Ident>,
    ) -> NFAttrTuple {
        let is_unnamed = named.is_none();
        if IGNORE_UNNAMED && is_unnamed {
            self.start_ignoring();
        }
        let result = match expr {
            ParserExpr::ParserRef(x) => {
                let tag = Tag::Toplevel(x.clone());
                if self.ignoring() {
                    (tag, None, AbstractType::unit_type())
                } else {
                    let ty = self
                        .annotations
                        .get(&tag)
                        .map(Rc::clone)
                        .map(AbstractType::Concrete)
                        .expect("toplevel rule must be typed");
                    (tag, named.or_else(|| Some(self.new_output_sym())), ty)
                }
            }
            ParserExpr::Ignore(expr) => {
                self.start_ignoring();
                let result = self.add_anonymous_rule::<IGNORE_UNNAMED>(expr, named);
                self.end_ignoring();
                result
            }
            _ => {
                let tag = self.new_anonymous_tag();
                let ty = self.add_nf_from_anonymous_expr(expr, &tag);
                if self.ignoring() {
                    (tag, None, AbstractType::unit_type())
                } else {
                    (tag, named.or_else(|| Some(self.new_output_sym())), ty)
                }
            }
        };
        if IGNORE_UNNAMED && is_unnamed {
            self.end_ignoring();
        }
        result
    }

    // Translate a top-level definition
    fn add_toplevel_def(&mut self, name: Ident, def: &ParserDef) {
        let tag = Tag::Toplevel(name);
        let rules = def
            .rules
            .iter()
            .map(|rule| {
                let semact = if let Some(action) = &rule.action {
                    SemAct::Customized(action.clone())
                } else if rule.vars.len() == 1 {
                    SemAct::infer(&rule.vars[0].expr)
                } else {
                    SemAct::Gather
                };
                let partial_nf = if matches!(semact, SemAct::Customized(..)) {
                    self.create_nf_from_sequence::<true, _>(
                        rule.vars
                            .iter()
                            .map(|binding| (&binding.expr, binding.name.clone())),
                        semact,
                        &tag,
                    )
                } else {
                    self.create_nf_from_sequence::<false, _>(
                        rule.vars
                            .iter()
                            .map(|binding| (&binding.expr, binding.name.clone())),
                        semact,
                        &tag,
                    )
                };
                partial_nf
            })
            .collect();
        self.semi_nfs.insert(tag, rules);
    }
}

#[cfg(test)]
mod test {
    use crate::frontend::Ast;

    use super::Translation;

    #[test]
    fn sexpr() {
        let ast = syn::parse_str::<Ast>(
            r#"
            %entry = sexp;

            DIGIT  = '0'..'9';
            ALPHA  = 'a'..'z' | 'A'..'Z';
            LPAREN = "(";
            RPAREN = ")";
            ATOM   = ALPHA (ALPHA | DIGIT)*;
            %skip  = (" " | "\t" | "\n" | "\r")+;

            compound: SExp = LPAREN sexp+[sexp:Vec<_>] RPAREN { SExp::Compound(sexp) };
            atom    : SExp = ATOM[atom] { SExp::Atom(atom) };
            sexp    : SExp = compound
                           | atom;
            "#,
        )
        .unwrap();
        #[cfg(feature = "debug")]
        {
            let translation = Translation::from(&ast);
            println!("{}", translation.semi_nfs);
        }
        #[cfg(not(feature = "debug"))]
        let _ = Translation::from(&ast);
    }
}
