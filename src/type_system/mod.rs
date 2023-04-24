use crate::{unreachable_branch, Location, Term, Token, UniqueSymbol};
use std::collections::HashSet;
use std::fmt::Debug;
use std::rc::Rc;

use crate::type_system::context::Context;
use thiserror::Error;

mod context;

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("type {0} and {1} violates the sequential uniqueness requirement")]
    SequentialUniquenessViolation(String, String),
    #[error("type {0} and {1} violates the disjunctive uniqueness requirement")]
    DisjunctiveUniquenessViolation(String, String),
    #[error("no such variable {0}")]
    NoSuchVariable(UniqueSymbol),
    #[error("unguarded fixpoint {0}")]
    UnguardedFixpoint(String),
    #[error("{1} at {0}")]
    Positioned(Location, Box<Self>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Type<T: Token> {
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
    pub fn fixpoint<E, F>(mut f: F) -> Result<Self, E>
    where
        F: FnMut(&Self) -> Result<Self, E>,
    {
        let mut last = Self::minimum();
        loop {
            let next = f(&last)?;
            if next == last {
                return Ok(next);
            }
            last = next;
        }
    }
}
pub fn well_typed<T: Token>(
    ctx: &mut Context<T>,
    term: Rc<Term<T>>,
) -> Result<Rc<Term<T>>, TypeError> {
    Ok(match term.as_ref() {
        Term::Epsilon => Rc::new(Term::WellTyped(term.clone(), Type::epsilon())),
        Term::Sequence(x, y) => {
            let x = well_typed(ctx, x.clone())?;
            let y = ctx.guarded(|ctx| well_typed(ctx, y.clone()))?;
            let x_type = match x.as_ref() {
                Term::WellTyped(_, t) => t,
                _ => unreachable_branch(),
            };
            let y_type = match y.as_ref() {
                Term::WellTyped(_, t) => t,
                _ => unreachable_branch(),
            };
            let r#type = Type::sequence(x_type, y_type)?;
            Rc::new(Term::WellTyped(Rc::new(Term::Sequence(x, y)), r#type))
        }
        Term::Token(x) => {
            let r#type = Type::token(x.clone());
            Rc::new(Term::WellTyped(term.clone(), r#type))
        }
        Term::Bottom => {
            let r#type = Type::bottom();
            Rc::new(Term::WellTyped(term.clone(), r#type))
        }
        Term::Alternative(x, y) => {
            let x = well_typed(ctx, x.clone())?;
            let y = well_typed(ctx, y.clone())?;
            let x_type = match x.as_ref() {
                Term::WellTyped(_, t) => t,
                _ => unreachable_branch(),
            };
            let y_type = match y.as_ref() {
                Term::WellTyped(_, t) => t,
                _ => unreachable_branch(),
            };
            let r#type = Type::alternative(x_type, y_type)?;
            Rc::new(Term::WellTyped(Rc::new(Term::Alternative(x, y)), r#type))
        }
        Term::Fix(var, body) => {
            let r#type = Type::fixpoint(|x| {
                ctx.with(var.clone(), x.clone(), |ctx| {
                    well_typed(ctx, body.clone()).map(|x| match x.as_ref() {
                        Term::WellTyped(_, t) => t.clone(),
                        _ => unreachable_branch(),
                    })
                })
            })?;
            if r#type.guarded {
                let well_typed_body = ctx.with(var.clone(), r#type.clone(), |ctx| {
                    well_typed(ctx, body.clone())
                })?;
                Rc::new(Term::WellTyped(
                    Rc::new(Term::Fix(var.clone(), well_typed_body)),
                    r#type,
                ))
            } else {
                return Err(TypeError::UnguardedFixpoint(format!("{:?}", r#type)));
            }
        }
        Term::Variable(name) => {
            let r#type = ctx
                .lookup(name)
                .ok_or(TypeError::NoSuchVariable(name.clone()))?;
            Rc::new(Term::WellTyped(term.clone(), r#type.into_owned()))
        }
        Term::SrcPos(inner, loc) => well_typed(ctx, inner.clone())
            .map_err(|x| TypeError::Positioned(loc.clone(), Box::new(x)))?,
        Term::WellTyped(_, _) => term.clone(),
    })
}
