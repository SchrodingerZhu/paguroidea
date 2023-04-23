use crate::type_system::Type;
use crate::{Token, UniqueSymbol};

use std::collections::{HashMap, HashSet};

pub struct Context<T: Token> {
    guarded: bool,
    gamma: HashMap<UniqueSymbol, Type<T>>,
}

pub enum TypeLookup<'a, T: Token> {
    Original(&'a Type<T>),
    Guarded(&'a Type<T>),
}

impl<'a, T: Token> TypeLookup<'a, T> {
    pub fn guarded(&self) -> bool {
        match self {
            TypeLookup::Original(x) => x.guarded,
            TypeLookup::Guarded(_) => true,
        }
    }
    pub fn first(&self) -> &HashSet<T> {
        match self {
            TypeLookup::Original(x) | TypeLookup::Guarded(x) => &x.first,
        }
    }
    pub fn follow(&self) -> &HashSet<T> {
        match self {
            TypeLookup::Original(x) | TypeLookup::Guarded(x) => &x.follow,
        }
    }
    pub fn nullable(&self) -> bool {
        match self {
            TypeLookup::Original(x) | TypeLookup::Guarded(x) => x.nullable,
        }
    }
}

impl<T: Token> Context<T> {
    pub fn new() -> Self {
        Self {
            guarded: false,
            gamma: HashMap::new(),
        }
    }
    pub fn lookup(&self, sym: &UniqueSymbol) -> Option<TypeLookup<T>> {
        let target = self.gamma.get(sym)?;
        Some(if self.guarded {
            TypeLookup::Guarded(target)
        } else {
            TypeLookup::Original(target)
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
