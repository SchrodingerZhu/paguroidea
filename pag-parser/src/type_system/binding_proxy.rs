use std::collections::HashSet;

use crate::{
    core_syntax::{BindingContext, Term},
    frontend::WithSpan,
    utilities::Symbol,
};

pub struct BindingProxy<'src, 'a> {
    binding: &'a BindingContext<'src, 'a>,
    hiding: HashSet<Symbol<'src>>,
}

impl<'src, 'a> BindingProxy<'src, 'a> {
    pub fn proxy(binding: &'a BindingContext<'src, 'a>) -> Self {
        BindingProxy {
            binding,
            hiding: HashSet::new(),
        }
    }
    pub fn lookup(&self, sym: &Symbol<'src>) -> Option<&'a WithSpan<'src, Term<'src, 'a>>> {
        if self.hiding.contains(sym) {
            return None;
        }
        self.binding.get(sym).map(|x| x.term)
    }
    pub fn with_hiding<F, R>(&mut self, sym: Symbol<'src>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let hidden_at_this_layer = self.hiding.insert(sym);
        let result = f(self);
        if hidden_at_this_layer {
            self.hiding.remove(&sym);
        }
        result
    }
}
