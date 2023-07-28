//!
//! Transform from surface syntax to semi-normalized form
//!

use std::collections::HashMap;

use quote::format_ident;
use syn::{Ident, Type};

use crate::frontend::{ParserDef, ParserExpr};

use super::{semact::SemAct, NormalForm, Tag, Action};

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
}

impl Translation {
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
    fn construct_actions_from_expr_sequence<'a, I>(&mut self, stream: I) -> Vec<Action> 
    where
        I: Iterator<Item = (&'a ParserExpr, Option<Ident>)>,
    {
        stream.map(|(expr, output )| {
            match expr {
                ParserExpr::ParserRef(rule) => 
                    Action::Shift { tag: Tag::Toplevel(rule.clone()), output: output.or_else(|| Some(self.new_output_sym())) },
                ParserExpr::LexerRef(_)  => 
                    Action::Shift { tag: self.add_anonymous_rule(expr), output }, 
                ParserExpr::Ignore(inner)  => 
                    Action::Shift { tag: self.add_anonymous_rule(inner), output: None }, 
                ParserExpr::Hinted( inner, ty) => {
                    let tag = self.add_anonymous_rule(inner);
                    self.hints.insert(tag.clone(), ty.clone());
                    Action::Shift { tag, output: None }
                }
                _ => 
                    Action::Shift { tag: self.add_anonymous_rule(expr), output: output.or_else(|| Some(self.new_output_sym())) },    
            }
        }).collect()
    }
    fn construct_nf_from_expr_sequence<'a, I>(&mut self, mut stream: I, semact: SemAct) -> NormalForm
    where
        I: Iterator<Item = (&'a ParserExpr, Option<Ident>)>,
    {
        let head = stream.next();
        match head {
            None => NormalForm::Empty(vec![], semact),
            // Token rule is ignored on default, but can be used to specify the label.
            Some((ParserExpr::LexerRef(token), label)) => {
                let actions = self.construct_actions_from_expr_sequence(stream);
                NormalForm::Sequence(token.clone(), label, actions, semact)
            }
            Some(_) => {
                let recovered = head.into_iter().chain(stream);
                let actions = self.construct_actions_from_expr_sequence(recovered);
                NormalForm::Unexpanded(actions, semact)
            }
        }
    }

    fn add_anonymous_rule(&mut self, expr: &ParserExpr) -> Tag {
        // Must be primitive rules

        let tag = self.new_anonymous_tag();
        let semact = SemAct::infer(expr);
        
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
