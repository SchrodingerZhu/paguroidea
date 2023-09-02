use crate::frontend::ParserExpr;

use super::Type;
use std::borrow::Cow;
use std::collections::HashMap;


#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Uid {
    Id(syn::Ident),
    Pt(*const ParserExpr) 
}
pub(super) struct TypeContext {
    guarded: bool,
    gamma: HashMap<Uid, Type>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            guarded: false,
            gamma: HashMap::new(),
        }
    }
    pub fn lookup(&self, sym: Uid) -> Option<Cow<Type>> {
        let target = self.gamma.get(&sym)?;
        Some(if self.guarded {
            Cow::Owned(Type {
                guarded: true,
                ..target.clone()
            })
        } else {
            Cow::Borrowed(target)
        })
    }
    pub fn guarded<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let backup = self.guarded;
        self.guarded = true;
        let result = f(self);
        self.guarded = backup;
        result
    }
    pub fn with<F, R>(&mut self, sym: Uid, r#type: Type, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let backup = self.gamma.insert(sym.clone(), r#type);
        let result = f(self);
        if let Some(backup) = backup {
            self.gamma.insert(sym, backup);
        } else {
            self.gamma.remove(&sym);
        }
        result
    }
}
