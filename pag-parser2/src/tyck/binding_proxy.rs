use std::collections::{HashSet, HashMap};
use syn::Ident;
use crate::{
    frontend::{ParserDef,ParserRule}
};

pub struct BindingProxy<'a> {
    pub binding: &'a HashMap<Ident, ParserDef>,
    hiding: HashSet<Ident>,
}

impl<'a> BindingProxy<'a> {
    pub fn proxy(binding: &'a HashMap<Ident, ParserDef>) -> Self {
        BindingProxy {
            binding,
            hiding: HashSet::new(),
        }
    }
    pub fn lookup(&self, sym: &Ident) -> Option<&Vec<ParserRule>> {
        if self.hiding.contains(sym) {
            return None;
        }
        self.binding.get(sym).map(|x| &x.rules)
    }
    pub fn with_hiding<F, R>(&mut self, sym: Ident, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let hidden_at_this_layer = self.hiding.insert(sym.clone());
        let result = f(self);
        if hidden_at_this_layer {
            self.hiding.remove(&sym);
        }
        result
    }
}
