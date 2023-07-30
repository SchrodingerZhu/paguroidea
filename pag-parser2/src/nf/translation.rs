//!
//! Transform from surface syntax to semi-normalized form
//!

use std::collections::HashMap;

use quote::format_ident;
use syn::{Ident, Type};

use super::NFTable;
use super::{semact::SemAct, Action, NormalForm, Tag};
use crate::frontend::RightDeepIterator;
use crate::frontend::{ParserDef, ParserExpr};

#[derive(Default)]
struct Translation {
    /// Table of semi-normalized production rules
    semi_nfs: NFTable,
    /// Toplevel type annotations
    annotations: HashMap<Tag, Type>,
    /// Type hints when calling inner routines (collector)
    hints: HashMap<Tag, Type>,
    /// Counter of assigned non-explicit variable names
    output_cnt: usize,
    /// Counter of assigned anonymous routines
    anonymous_cnt: usize,
    /// Whether we are currently ignoring the output
    ignoring_cnt: usize,
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

    /// Construct a normal form from a sequence of parser expressions. The semact is always `Recognize`.
    fn partial_nf_from_sequence<
        'a,
        const IGNORE_UNNAMED: bool,
        I: Iterator<Item = (&'a ParserExpr, Option<Ident>)>,
    >(
        &mut self,
        mut iter: I,
    ) -> NormalForm {
        match iter.next() {
            None => NormalForm::Empty(vec![], SemAct::Recognize),
            Some((ParserExpr::Ignore(box ParserExpr::LexerRef(token)), _)) => {
                let tail = iter
                    .map(|(inner, named)| self.add_anonymous_rule::<IGNORE_UNNAMED>(inner, named))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                NormalForm::Sequence(token.clone(), None, tail, SemAct::Recognize)
            }
            Some((ParserExpr::LexerRef(token), named)) => {
                let tail = iter
                    .map(|(inner, named)| self.add_anonymous_rule::<IGNORE_UNNAMED>(inner, named))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                NormalForm::Sequence(
                    token.clone(),
                    if self.ignoring() {
                        None
                    } else if IGNORE_UNNAMED {
                        named
                    } else {
                        named.or_else(|| Some(self.new_output_sym()))
                    },
                    tail,
                    SemAct::Recognize,
                )
            }
            Some((expr, named)) => {
                let sequence = [(expr, named)]
                    .into_iter()
                    .chain(iter)
                    .map(|(inner, named)| self.add_anonymous_rule::<IGNORE_UNNAMED>(inner, named))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                NormalForm::Unexpanded(sequence, SemAct::Recognize)
            }
        }
    }

    fn add_nf(&mut self, tag: Tag, nf: NormalForm) {
        self.semi_nfs.entry(tag).or_default().push(nf);
    }

    fn add_nf_from_anonymous_expr(&mut self, expr: &ParserExpr, tag: &Tag) {
        match expr {
            ParserExpr::Seq(..) => {
                let mut partial_nf = self.partial_nf_from_sequence::<false, _>(
                    RightDeepIterator::from(expr).map(|expr| (expr, None)),
                );
                *partial_nf.semact_mut() = if self.ignoring() {
                    SemAct::Recognize
                } else {
                    SemAct::Gather
                };
                self.add_nf(tag.clone(), partial_nf);
            }
            ParserExpr::Opt(inner) => {
                let mut partial_nf = self.partial_nf_from_sequence::<false, _>(
                    RightDeepIterator::from(inner.as_ref()).map(|expr| (expr, None)),
                );
                let semact = if self.ignoring() {
                    SemAct::Recognize
                } else {
                    SemAct::Option
                };
                *partial_nf.semact_mut() = semact.clone();
                self.add_nf(tag.clone(), partial_nf);
                // add one more rule for empty
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], semact));
            }
            ParserExpr::Star(inner) => {
                let mut partial_nf = self.partial_nf_from_sequence::<false, _>(
                    RightDeepIterator::from(inner.as_ref()).map(|expr| (expr, None)),
                );
                let semact = if self.ignoring() {
                    SemAct::Recognize
                } else {
                    SemAct::ZeroOrMore
                };
                *partial_nf.semact_mut() = semact.clone();
                self.add_nf(tag.clone(), partial_nf);
                // add one more rule for empty
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], semact));
            }
            ParserExpr::Plus(inner) => {
                let mut partial_nf = self.partial_nf_from_sequence::<false, _>(
                    RightDeepIterator::from(inner.as_ref()).map(|expr| (expr, None)),
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
                        nf.append_tailcall();
                        *nf.semact_mut() = semact.clone();
                        nf
                    });

                    self.add_nf(nested_tag.clone(), NormalForm::Empty(vec![], semact));
                }
                // the toplevel routine
                {
                    let semact = if self.ignoring() {
                        SemAct::Recognize
                    } else {
                        SemAct::OneOrMoreToplevel
                    };
                    partial_nf.append_pass_collector(nested_tag);
                    *partial_nf.semact_mut() = semact;
                    self.add_nf(tag.clone(), partial_nf);
                }
            }
            ParserExpr::LexerRef(ident) => {
                let nf = if self.ignoring() {
                    NormalForm::Sequence(ident.clone(), None, vec![], SemAct::Recognize)
                } else {
                    NormalForm::Sequence(
                        ident.clone(),
                        Some(self.new_output_sym()),
                        vec![],
                        SemAct::Token,
                    )
                };
                self.add_nf(tag.clone(), nf);
            }
            ParserExpr::ParserRef(_) => unreachable!("cannot create nf from parser ref"),
            ParserExpr::Ignore(_) => unreachable!("cannot create nf from ignore"),
        }
    }

    fn add_anonymous_rule<const IGNORE_UNNAMED: bool>(
        &mut self,
        expr: &ParserExpr,
        named: Option<Ident>,
    ) -> (Tag, Option<Ident>) {
        let is_unnamed = named.is_none();
        if IGNORE_UNNAMED && is_unnamed {
            self.start_ignoring();
        }
        let result = match expr {
            ParserExpr::ParserRef(x) => {
                let tag = Tag::Toplevel(x.clone());
                if self.ignoring() {
                    (tag, None)
                } else {
                    (tag, named.or_else(|| Some(self.new_output_sym())))
                }
            }
            ParserExpr::Ignore(expr) => {
                self.start_ignoring();
                let (tag, output) = self.add_anonymous_rule::<IGNORE_UNNAMED>(expr, named);
                self.end_ignoring();
                (tag, output)
            }
            _ => {
                let tag = self.new_anonymous_tag();
                self.add_nf_from_anonymous_expr(expr, &tag);
                if self.ignoring() {
                    (tag, None)
                } else {
                    (tag, named.or_else(|| Some(self.new_output_sym())))
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
        self.annotations.insert(tag.clone(), def.ty.clone());
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
                let mut partial_nf = if matches!(semact, SemAct::Customized(..)) {
                    self.partial_nf_from_sequence::<true, _>(
                        rule.vars
                            .iter()
                            .map(|binding| (&binding.expr, binding.name.clone())),
                    )
                } else {
                    self.partial_nf_from_sequence::<false, _>(
                        rule.vars
                            .iter()
                            .map(|binding| (&binding.expr, binding.name.clone())),
                    )
                };
                *partial_nf.semact_mut() = semact;
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
        let mut translation = Translation::default();
        for (name, def) in ast.parser_map.iter() {
            translation.add_toplevel_def(name.clone(), def);
        }
        #[cfg(feature = "debug")]
        println!("{}", translation.semi_nfs);
    }
}
