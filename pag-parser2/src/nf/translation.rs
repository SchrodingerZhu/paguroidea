//!
//! Transform from surface syntax to semi-normalized form
//!

use std::collections::HashMap;

use quote::format_ident;
use syn::{Ident, Type};

use crate::frontend::{ParserDef};

use super::{semact::SemAct, NormalForm, Tag};

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
    // Translate a top-level definitioin
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
