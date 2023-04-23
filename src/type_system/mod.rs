use crate::Token;
use std::collections::HashSet;
use std::fmt::Debug;

use thiserror::Error;

mod context;

#[derive(Error, Debug)]
enum TypeError {
    #[error("type {0} and {1} violates the sequential uniqueness requirement")]
    SequentialUniquenessViolation(String, String),
    #[error("type {0} and {1} violates the disjunctive uniqueness requirement")]
    DisjunctiveUniquenessViolation(String, String),
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct Type<T: Token> {
    first: HashSet<T>,
    follow: HashSet<T>,
    nullable: bool,
    guarded: bool,
}

impl<T> Type<T>
where
    T: Token,
{
    pub fn sequential_uniqueness(&self, other: &Self) -> bool {
        !self.nullable && self.follow.is_disjoint(&other.first)
    }
    pub fn disjunctive_uniqueness(&self, other: &Self) -> bool {
        !(self.nullable && other.nullable) && self.first.is_disjoint(&other.first)
    }
    pub fn epsilon() -> Self {
        Self {
            first: HashSet::new(),
            follow: HashSet::new(),
            nullable: true,
            guarded: true,
        }
    }
    pub fn token(token: T) -> Self {
        Self {
            first: HashSet::from([token]),
            follow: HashSet::new(),
            nullable: false,
            guarded: true,
        }
    }
    pub fn sequence(t1: &Self, t2: &Self) -> Result<Self, TypeError> {
        if t1.sequential_uniqueness(t2) {
            Ok(Self {
                first: t1.first.clone(),
                follow: if t2.nullable {
                    t2.follow
                        .union(&t2.first)
                        .chain(t1.follow.iter())
                        .cloned()
                        .collect()
                } else {
                    t2.follow.clone()
                },
                nullable: false,
                guarded: t1.guarded,
            })
        } else {
            Err(TypeError::SequentialUniquenessViolation(
                format!("{t1:?}"),
                format!("{t2:?}"),
            ))
        }
    }
    pub fn bottom() -> Self {
        Self {
            first: HashSet::new(),
            follow: HashSet::new(),
            nullable: false,
            guarded: true,
        }
    }
    pub fn alternative(t1: &Self, t2: &Self) -> Result<Self, TypeError> {
        if t1.disjunctive_uniqueness(t2) {
            Ok(Self {
                first: t1.first.union(&t2.first).cloned().collect(),
                follow: t1.follow.union(&t2.follow).cloned().collect(),
                nullable: t1.nullable || t2.nullable,
                guarded: t1.guarded && t2.guarded,
            })
        } else {
            Err(TypeError::DisjunctiveUniquenessViolation(
                format!("{t1:?}"),
                format!("{t2:?}"),
            ))
        }
    }
    pub fn star(t: &Self) -> Result<Self, TypeError> {
        if t.sequential_uniqueness(t) {
            Ok(Self {
                first: t.first.clone(),
                follow: t.first.union(&t.follow).cloned().collect(),
                nullable: true,
                guarded: t.guarded,
            })
        } else {
            Err(TypeError::SequentialUniquenessViolation(
                format!("{t:?}"),
                format!("{t:?}"),
            ))
        }
    }
    fn minimum() -> Self {
        Self {
            first: HashSet::new(),
            follow: HashSet::new(),
            nullable: false,
            guarded: false,
        }
    }
    pub fn fixpoint<F>(f: F) -> Self
    where
        F: Fn(&Self) -> Self,
    {
        let mut last = Self::minimum();
        loop {
            let next = f(&last);
            if next == last {
                return next;
            }
            last = next;
        }
    }
}
