use crate::type_system::Type;
use crate::{Token, UniqueSymbol};
use std::borrow::Cow;

use std::collections::HashMap;

pub struct Context<T: Token> {
    guarded: bool,
    gamma: HashMap<UniqueSymbol, Type<T>>,
}

impl<T: Token> Context<T> {
    pub fn new() -> Self {
        Self {
            guarded: false,
            gamma: HashMap::new(),
        }
    }
    pub fn lookup(&self, sym: &UniqueSymbol) -> Option<Cow<Type<T>>> {
        let target = self.gamma.get(sym)?;
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
    pub fn with<F, R>(&mut self, sym: UniqueSymbol, r#type: Type<T>, f: F) -> R
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
