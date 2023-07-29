//!
//! Transform from surface syntax to semi-normalized form
//!

use std::collections::HashMap;

use quote::format_ident;
use syn::{Ident, Type};

use super::{semact::SemAct, Action, NormalForm, Tag};
use crate::frontend::RightDeepIterator;
use crate::frontend::{ParserDef, ParserExpr};

struct Translation {
    /// Table of semi-normalized production rules
    semi_nfs: HashMap<Tag, Vec<NormalForm>>,
    /// Toplevel type annotations
    annotations: HashMap<Tag, Type>,
    /// Type hints when calling inner routines (collector)
    hints: HashMap<Tag, Type>,
    /// Counter of assigned non-explicit variable names
    output_cnt: usize,
    /// Counter of assigned anonymous routines
    anonymous_cnt: usize,
    /// Whether we are currently ignoring the output
    ignoring: bool,
}

impl Translation {
    fn start_ignoring(&mut self) {
        self.ignoring = true;
    }
    fn end_ignore(&mut self) {
        self.ignoring = false;
    }

    // Allocate a new symbol for unamed variable bindings.
    fn new_output_sym(&mut self) -> Ident {
        let result = format_ident!("_{}", self.output_cnt);
        self.output_cnt += 1;
        result
    }
    // Allocate a new tag for anonymous routines.
    fn new_anonymous_tag(&mut self) -> Tag {
        let result = Tag::Anonymous(self.anonymous_cnt);
        self.anonymous_cnt += 1;
        result
    }
    fn add_nf(&mut self, tag: Tag, nf: NormalForm) {
        self.semi_nfs.entry(tag).or_default().push(nf);
    }
    fn add_nf_from_anonymous_expr(&mut self, expr: &ParserExpr, tag: &Tag) {
        match expr {
            ParserExpr::Seq(box ParserExpr::Ignore(box ParserExpr::LexerRef(head)), tail) => {
                let tail_actions = RightDeepIterator::from(tail.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                self.add_nf(
                    tag.clone(),
                    NormalForm::Sequence(
                        head.clone(),
                        None,
                        tail_actions,
                        if self.ignoring {
                            SemAct::Recognize
                        } else {
                            SemAct::Gather
                        },
                    ),
                );
            }
            ParserExpr::Seq(box ParserExpr::LexerRef(head), tail) => {
                let tail_actions = RightDeepIterator::from(tail.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                let nf = NormalForm::Sequence(
                    head.clone(),
                    if self.ignoring {
                        None
                    } else {
                        Some(self.new_output_sym())
                    },
                    tail_actions,
                    if self.ignoring {
                        SemAct::Recognize
                    } else {
                        SemAct::Gather
                    },
                );
                self.add_nf(tag.clone(), nf);
            }
            ParserExpr::Seq(_, _) => {
                let actions = RightDeepIterator::from(expr)
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                let nf = NormalForm::Unexpanded(
                    actions,
                    if self.ignoring {
                        SemAct::Recognize
                    } else {
                        SemAct::Gather
                    },
                );
                self.add_nf(tag.clone(), nf);
            }
            ParserExpr::Opt(box ParserExpr::Seq(
                box ParserExpr::Ignore(box ParserExpr::LexerRef(head)),
                tail,
            )) => {
                let tail_actions = RightDeepIterator::from(tail.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                self.add_nf(
                    tag.clone(),
                    NormalForm::Sequence(
                        head.clone(),
                        None,
                        tail_actions,
                        if self.ignoring {
                            SemAct::Recognize
                        } else {
                            SemAct::Option
                        },
                    ),
                );
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::Option));
            }
            ParserExpr::Opt(box ParserExpr::Seq(box ParserExpr::LexerRef(head), tail)) => {
                let tail_actions = RightDeepIterator::from(tail.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                let nf = NormalForm::Sequence(
                    head.clone(),
                    if self.ignoring {
                        None
                    } else {
                        Some(self.new_output_sym())
                    },
                    tail_actions,
                    if self.ignoring {
                        SemAct::Recognize
                    } else {
                        SemAct::Option
                    },
                );
                self.add_nf(tag.clone(), nf);
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::Option));
            }
            ParserExpr::Opt(inner) => {
                let actions = RightDeepIterator::from(inner.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                let nf = NormalForm::Unexpanded(
                    actions,
                    if self.ignoring {
                        SemAct::Recognize
                    } else {
                        SemAct::Option
                    },
                );
                self.add_nf(tag.clone(), nf);
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::Option));
            }
            ParserExpr::Star(box ParserExpr::Seq(
                box ParserExpr::Ignore(box ParserExpr::LexerRef(head)),
                tail,
            )) => {
                let tail_actions = RightDeepIterator::from(tail.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                self.add_nf(
                    tag.clone(),
                    NormalForm::Sequence(
                        head.clone(),
                        None,
                        tail_actions,
                        if self.ignoring {
                            SemAct::Recognize
                        } else {
                            SemAct::ZeroOrMore
                        },
                    ),
                );
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::ZeroOrMore));
            }
            ParserExpr::Star(box ParserExpr::Seq(box ParserExpr::LexerRef(head), tail)) => {
                let tail_actions = RightDeepIterator::from(tail.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                let nf = NormalForm::Sequence(
                    head.clone(),
                    if self.ignoring {
                        None
                    } else {
                        Some(self.new_output_sym())
                    },
                    tail_actions,
                    if self.ignoring {
                        SemAct::Recognize
                    } else {
                        SemAct::ZeroOrMore
                    },
                );
                self.add_nf(tag.clone(), nf);
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::ZeroOrMore));
            }
            ParserExpr::Star(inner) => {
                let actions = RightDeepIterator::from(inner.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                let nf = NormalForm::Unexpanded(
                    actions,
                    if self.ignoring {
                        SemAct::Recognize
                    } else {
                        SemAct::ZeroOrMore
                    },
                );
                self.add_nf(tag.clone(), nf);
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::ZeroOrMore));
            }
            ParserExpr::Plus(box ParserExpr::Seq(
                box ParserExpr::Ignore(box ParserExpr::LexerRef(head)),
                tail,
            )) => {
                let tail_actions = RightDeepIterator::from(tail.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                self.add_nf(
                    tag.clone(),
                    NormalForm::Sequence(
                        head.clone(),
                        None,
                        tail_actions,
                        if self.ignoring {
                            SemAct::Recognize
                        } else {
                            SemAct::OneOrMoreToplevel
                        },
                    ),
                );
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::ZeroOrMore));
            }
            ParserExpr::Plus(box ParserExpr::Seq(box ParserExpr::LexerRef(head), tail)) => {
                let tail_actions = RightDeepIterator::from(tail.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                let nf = NormalForm::Sequence(
                    head.clone(),
                    if self.ignoring {
                        None
                    } else {
                        Some(self.new_output_sym())
                    },
                    tail_actions,
                    if self.ignoring {
                        SemAct::Recognize
                    } else {
                        SemAct::ZeroOrMore
                    },
                );
                self.add_nf(tag.clone(), nf);
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::ZeroOrMore));
            }
            ParserExpr::Plus(inner) => {
                let actions = RightDeepIterator::from(inner.as_ref())
                    .map(|inner| self.add_anonymous_rule(inner))
                    .map(|(tag, output)| Action::Shift { tag, output })
                    .collect();
                let nf = NormalForm::Unexpanded(
                    actions,
                    if self.ignoring {
                        SemAct::Recognize
                    } else {
                        SemAct::ZeroOrMore
                    },
                );
                self.add_nf(tag.clone(), nf);
                self.add_nf(tag.clone(), NormalForm::Empty(vec![], SemAct::ZeroOrMore));
            }
            ParserExpr::LexerRef(ident) => {
                let nf = if self.ignoring {
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
            ParserExpr::Hinted(_, _) => unreachable!("cannot create nf from hinted"),
        }
    }
    fn add_anonymous_rule(&mut self, expr: &ParserExpr) -> (Tag, Option<Ident>) {
        todo!()
    }

    // Translate a top-level definition
    fn add_toplevel_def(&mut self, name: Ident, def: &ParserDef) {
        let tag = Tag::Toplevel(name);
        self.annotations.insert(tag.clone(), def.ty.clone());
        let rules = def
            .rules
            .iter()
            .map(|rule| {
                let semact = if let Some(x) = rule.action.clone() {
                    SemAct::CustomizedRoutine(x)
                } else if rule.vars.len() == 1 {
                    SemAct::infer(&rule.vars[0].expr)
                } else {
                    SemAct::Gather
                };
                match semact {
                    SemAct::Gather | SemAct::CustomizedRoutine(_) => {}
                    _ => {}
                }
                todo!()
            })
            .collect();
        self.semi_nfs.insert(tag, rules);
    }
}
