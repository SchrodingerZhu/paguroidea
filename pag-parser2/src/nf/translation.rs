//! Transform from surface syntax to semi-normalized form

use std::collections::HashMap;
use std::rc::Rc;

use quote::format_ident;
use syn::{Ident, Type};

use super::{semact::SemAct, Action, NormalForm, Tag};
use super::{AbstractType, NFTable};
use crate::frontend::{Ast, ParserDef, ParserExpr};

pub struct Translation {
    /// Table of semi-normalized production rules
    pub semi_nfs: NFTable,
    /// Toplevel type annotations
    annotations: HashMap<Tag, Rc<Type>>,
    /// Type hints when calling inner routines (collector)
    pub hints: HashMap<Tag, Rc<Type>>,
    /// Counter of assigned non-explicit variable names
    output_cnt: usize,
    /// Counter of assigned anonymous routines
    anonymous_cnt: usize,
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
        };
        for (i, def) in &value.parser_map {
            translation.add_toplevel_def(i.clone(), def);
        }
        translation
    }
}

impl Translation {
    fn add_nf(&mut self, tag: Tag, nf: NormalForm) {
        self.semi_nfs.entry(tag).or_default().push(nf);
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

    // Translate a top-level definition
    fn add_toplevel_def(&mut self, name: Ident, def: &ParserDef) {
        let tag = Tag::Toplevel(name);
        let rules = def
            .rules
            .iter()
            .map(|rule| {
                let semact = if let Some(action) = &rule.action {
                    SemAct::Customized(action.clone())
                } else {
                    SemAct::Gather
                };
                let ignore_unnamed = matches!(semact, SemAct::Customized(..));
                self.create_nf_from_sequence(
                    rule.vars
                        .iter()
                        .map(|binding| (&binding.expr, binding.name.clone(), binding.ty.clone())),
                    semact,
                    &tag,
                    ignore_unnamed,
                )
            })
            .collect();
        self.semi_nfs.insert(tag, rules);
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
                0 => AbstractType::unit(),
                1 => inner_types.next().unwrap(),
                _ => AbstractType::Tuple(inner_types.collect()),
            },
            SemAct::Recognize => AbstractType::unit(),
            _ => unreachable!(),
        }
    }

    /// Construct a normal form from a sequence of parser expressions.
    fn create_nf_from_sequence<
        'a,
        I: Iterator<Item = (&'a ParserExpr, Option<Ident>, Option<Rc<Type>>)>,
    >(
        &mut self,
        mut iter: I,
        semact: SemAct,
        tag: &Tag,
        ignore_unnamed: bool,
    ) -> NormalForm {
        match iter.next().unwrap() {
            (ParserExpr::Ignore(box ParserExpr::LexerRef(token)), _, _) => {
                let mut types = Vec::new();
                let actions = iter
                    .map(|(inner, name, hint)| {
                        let is_ignored = ignore_unnamed && name.is_none();
                        let (tag, output, ty) =
                            self.add_anonymous_rule(inner, name, hint, is_ignored);
                        if output.is_some() {
                            types.push(ty);
                        }
                        Action::Shift { tag, output }
                    })
                    .collect();
                let ty = self.infer_type(types.into_iter(), &semact, tag).into();
                NormalForm::Sequence {
                    token: token.clone(),
                    actions,
                    semact,
                    ty,
                }
            }
            (ParserExpr::LexerRef(token), name, _) => {
                let mut types = Vec::new();
                if name.is_some() {
                    types.push(AbstractType::span())
                }
                let head_action =
                    if matches!(semact, SemAct::Recognize) || (ignore_unnamed && name.is_none()) {
                        None
                    } else {
                        Some(Action::Reduce {
                            semact: SemAct::Token,
                            hint: None,
                            output: name.or_else(|| Some(self.new_output_sym())),
                        })
                    };
                let actions = head_action
                    .into_iter()
                    .chain(iter.map(|(inner, name, hint)| {
                        let is_ignored = ignore_unnamed && name.is_none();
                        let (tag, output, ty) =
                            self.add_anonymous_rule(inner, name, hint, is_ignored);
                        if output.is_some() {
                            types.push(ty);
                        }
                        Action::Shift { tag, output }
                    }))
                    .collect();
                let ty = self.infer_type(types.into_iter(), &semact, tag).into();
                NormalForm::Sequence {
                    token: token.clone(),
                    actions,
                    semact,
                    ty,
                }
            }
            (expr, name, hint) => {
                let mut types = Vec::new();
                let actions = [(expr, name, hint)]
                    .into_iter()
                    .chain(iter)
                    .map(|(inner, name, hint)| {
                        let is_ignored = ignore_unnamed && name.is_none();
                        let (tag, output, ty) =
                            self.add_anonymous_rule(inner, name, hint, is_ignored);
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

    fn add_anonymous_rule(
        &mut self,
        expr: &ParserExpr,
        bind_name: Option<Ident>,
        bind_ty: Option<Rc<Type>>,
        is_ignored: bool,
    ) -> NFAttrTuple {
        let name = (!is_ignored).then(|| bind_name.unwrap_or_else(|| self.new_output_sym()));
        let recognize_or = |semact: SemAct| match is_ignored {
            true => SemAct::Recognize,
            false => semact,
        };
        let unit_or = |ty: AbstractType| match is_ignored {
            true => AbstractType::unit(),
            false => ty,
        };
        let (tag, name, ty) = match expr {
            ParserExpr::Seq(exprs) => {
                let self_tag = self.new_anonymous_tag();

                let partial_nf = self.create_nf_from_sequence(
                    exprs.iter().map(|expr| (expr, None, None)),
                    recognize_or(SemAct::Gather),
                    &self_tag,
                    false,
                );
                let ty = unit_or(partial_nf.ty().0.clone());
                self.add_nf(self_tag.clone(), partial_nf);

                (self_tag, name, ty)
            }
            ParserExpr::Star(inner) => {
                let self_tag = self.new_anonymous_tag();

                let (tag, output, ty) = self.add_anonymous_rule(inner, None, None, is_ignored);
                let ty = unit_or(AbstractType::Collector(Box::new(ty)));

                let nf_collect = NormalForm::Unexpanded {
                    actions: vec![Action::Shift { tag, output }],
                    semact: recognize_or(SemAct::ZeroOrMoreCollect),
                    ty: ty.clone().into(),
                };
                self.add_nf(self_tag.clone(), nf_collect);

                let nf_finish = NormalForm::Empty {
                    actions: vec![],
                    semact: recognize_or(SemAct::ZeroOrMoreFinish),
                    ty: ty.clone().into(),
                };
                self.add_nf(self_tag.clone(), nf_finish);

                (self_tag, name, ty)
            }
            ParserExpr::Plus(inner) => {
                let self_tag = self.new_anonymous_tag();
                let nested_tag = self.new_anonymous_tag();

                let (tag, output, ty) = self.add_anonymous_rule(inner, None, None, is_ignored);
                let action = Action::Shift { tag, output };
                let ty = unit_or(AbstractType::Collector(Box::new(ty)));

                let nf_collect = NormalForm::Unexpanded {
                    actions: vec![action.clone(), Action::TailCall],
                    semact: recognize_or(SemAct::OneOrMoreCollect),
                    ty: ty.clone().into(),
                };
                self.add_nf(nested_tag.clone(), nf_collect);

                let nf_finish = NormalForm::Empty {
                    actions: vec![],
                    semact: recognize_or(SemAct::OneOrMoreFinish),
                    ty: ty.clone().into(),
                };
                self.add_nf(nested_tag.clone(), nf_finish);

                let nf_toplevel = NormalForm::Unexpanded {
                    actions: vec![action, Action::PassCollector(nested_tag)],
                    semact: recognize_or(SemAct::OneOrMoreToplevel),
                    ty: ty.clone().into(),
                };
                self.add_nf(self_tag.clone(), nf_toplevel);

                (self_tag, name, ty)
            }
            ParserExpr::Opt(inner) => {
                let self_tag = self.new_anonymous_tag();

                let (tag, output, ty) = self.add_anonymous_rule(inner, None, None, is_ignored);
                let ty = unit_or(AbstractType::Option(Box::new(ty)));

                let nf_some = NormalForm::Unexpanded {
                    actions: vec![Action::Shift { tag, output }],
                    semact: recognize_or(SemAct::OptSome),
                    ty: ty.clone().into(),
                };
                self.add_nf(self_tag.clone(), nf_some);

                let nf_none = NormalForm::Empty {
                    actions: vec![],
                    semact: recognize_or(SemAct::OptNone),
                    ty: ty.clone().into(),
                };
                self.add_nf(self_tag.clone(), nf_none);

                (self_tag, name, ty)
            }
            ParserExpr::LexerRef(ident) => {
                let tag = self.new_anonymous_tag();
                let ty = unit_or(AbstractType::span());

                let nf = NormalForm::Sequence {
                    token: ident.clone(),
                    actions: vec![],
                    semact: recognize_or(SemAct::Token),
                    ty: ty.clone().into(),
                };
                self.add_nf(tag.clone(), nf);

                (tag, name, ty)
            }
            ParserExpr::ParserRef(ident) => {
                let tag = Tag::Toplevel(ident.clone());
                let ty = unit_or(AbstractType::Concrete(self.annotations[&tag].clone()));
                (tag, name, ty)
            }
            ParserExpr::Ignore(expr) => self.add_anonymous_rule(expr, None, None, true),
        };
        if let Some(x) = bind_ty {
            self.hints.insert(tag.clone(), x);
        }
        (tag, name, ty)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::frontend::Ast;

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
