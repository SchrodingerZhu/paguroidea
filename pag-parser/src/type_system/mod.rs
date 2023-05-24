// Copyright (c) 2023 Paguroidea Developpers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::core_syntax::{BindingContext, Term};
use crate::frontend::WithSpan;
use crate::utilities::Symbol;
use std::collections::HashSet;
use std::fmt::Debug;
use std::vec;

use crate::type_system::context::TypeContext;
use pest::Span;
use thiserror::Error;

use self::binding_proxy::BindingProxy;

pub mod binding_proxy;
pub mod context;

#[derive(Error, Debug)]
pub enum TypeError<'a> {
    #[error("sequential uniqueness requirement volidation")]
    SequentialUniquenessViolation {
        lhs: (Span<'a>, Type<'a>),
        rhs: (Span<'a>, Type<'a>),
        total: Span<'a>,
    },
    #[error("disjunctive uniqueness requirement volidation")]
    DisjunctiveUniquenessViolation {
        lhs: (Span<'a>, Type<'a>),
        rhs: (Span<'a>, Type<'a>),
        total: Span<'a>,
    },
    #[error("unguarded fixpoint {0}")]
    UnguardedFixpoint(Symbol<'a>, Span<'a>),
    #[error("unresolved reference {0}")]
    UnresolvedReference(Symbol<'a>, Span<'a>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Type<'src> {
    pub first: HashSet<Symbol<'src>>,
    pub follow: HashSet<Symbol<'src>>,
    pub nullable: bool,
    pub guarded: bool,
}

impl<'src> Type<'src> {
    fn sequential_uniqueness(&self, other: &Self) -> bool {
        !self.nullable && self.follow.is_disjoint(&other.first)
    }
    fn disjunctive_uniqueness(&self, other: &Self) -> bool {
        !(self.nullable && other.nullable) && self.first.is_disjoint(&other.first)
    }
    fn epsilon() -> Self {
        Self {
            first: HashSet::new(),
            follow: HashSet::new(),
            nullable: true,
            guarded: true,
        }
    }
    fn token(token: Symbol<'src>) -> Self {
        Self {
            first: HashSet::from([token]),
            follow: HashSet::new(),
            nullable: false,
            guarded: true,
        }
    }

    fn sequence(
        t1: &Self,
        t2: &Self,
        lhs: Span<'src>,
        rhs: Span<'src>,
        total: Span<'src>,
    ) -> Result<Self, Box<TypeError<'src>>> {
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
            // it is fine to return large error here, they will be stored in vectors anyway
            Err(Box::new(TypeError::SequentialUniquenessViolation {
                lhs: (lhs, t1.clone()),
                rhs: (rhs, t2.clone()),
                total,
            }))
        }
    }
    fn bottom() -> Self {
        Self {
            first: HashSet::new(),
            follow: HashSet::new(),
            nullable: false,
            guarded: true,
        }
    }

    fn alternative(
        t1: &Self,
        t2: &Self,
        lhs: Span<'src>,
        rhs: Span<'src>,
        total: Span<'src>,
    ) -> Result<Self, Box<TypeError<'src>>> {
        if t1.disjunctive_uniqueness(t2) {
            Ok(Self {
                first: t1.first.union(&t2.first).cloned().collect(),
                follow: t1.follow.union(&t2.follow).cloned().collect(),
                nullable: t1.nullable || t2.nullable,
                guarded: t1.guarded && t2.guarded,
            })
        } else {
            // it is fine to return large error here, they will be stored in vectors anyway
            Err(Box::new(TypeError::DisjunctiveUniquenessViolation {
                lhs: (lhs, t1.clone()),
                rhs: (rhs, t2.clone()),
                total,
            }))
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
    fn fixpoint<F>(mut f: F) -> (Self, Vec<TypeError<'src>>)
    where
        F: FnMut(&Self) -> (Self, Vec<TypeError<'src>>),
    {
        let mut last = Self::minimum();
        loop {
            let (next, errs) = f(&last);
            if !errs.is_empty() || next == last {
                return (next, errs);
            }
            last = next;
        }
    }
}

fn type_check_impl<'src, 'a>(
    typing_ctx: &mut TypeContext<'src>,
    binding_ctx: &mut BindingProxy<'src, 'a>,
    term: &'a WithSpan<'src, Term<'src, 'a>>,
) -> (Type<'src>, Vec<TypeError<'src>>) {
    match &term.node {
        Term::Epsilon => (Type::epsilon(), vec![]),
        Term::Sequence(x, y) => {
            let (x_type, x_errors) = type_check_impl(typing_ctx, binding_ctx, x);
            let (y_type, y_errors) = typing_ctx.guarded(|ctx| type_check_impl(ctx, binding_ctx, y));
            let (r#type, err) = match Type::sequence(&x_type, &y_type, x.span, y.span, term.span) {
                Ok(r#type) => (r#type, None),
                Err(err) => (Type::bottom(), Some(err)),
            };
            (
                r#type,
                x_errors
                    .into_iter()
                    .chain(y_errors)
                    .chain(err.map(Box::into_inner))
                    .collect(),
            )
        }
        Term::LexerRef(name) => (Type::token(*name), vec![]),
        Term::Bottom => (Type::bottom(), vec![]),
        Term::Alternative(x, y) => {
            let (x_type, x_errors) = type_check_impl(typing_ctx, binding_ctx, x);
            let (y_type, y_errors) = type_check_impl(typing_ctx, binding_ctx, y);
            let (r#type, err) = match Type::alternative(&x_type, &y_type, x.span, y.span, term.span)
            {
                Ok(r#type) => (r#type, None),
                Err(err) => (Type::bottom(), Some(err)),
            };
            (
                r#type,
                x_errors
                    .into_iter()
                    .chain(y_errors)
                    .chain(err.map(Box::into_inner))
                    .collect(),
            )
        }
        Term::ParserRef(name) => {
            // first check if name is already typed in the context.
            // if so return that type directly.
            if let Some(ty) = typing_ctx.lookup(*name) {
                return (ty.as_ref().clone(), vec![]);
            }
            // otherwise, we need to type check the parser definition.
            if let Some(target) = binding_ctx.lookup(name) {
                // we should not cache the result, since it can be recursive and changed during the calculation of the fixpoint.
                let (r#type, errors) = binding_ctx.with_hiding(*name, |binding_ctx| {
                    type_check_impl(typing_ctx, binding_ctx, target)
                });
                (r#type, errors)
            } else {
                (
                    Type::bottom(),
                    vec![TypeError::UnresolvedReference(*name, term.span)],
                )
            }
        }
        Term::Fix(var, body) => {
            if let Some(ty) = typing_ctx.lookup(*var) {
                return (ty.as_ref().clone(), vec![]);
            }
            let (r#type, errs) = Type::fixpoint(|x| {
                typing_ctx.with(*var, x.clone(), |ctx| {
                    type_check_impl(ctx, binding_ctx, body)
                })
            });
            if !errs.is_empty() {
                return (r#type, errs);
            }
            if r#type.guarded {
                typing_ctx.with(*var, r#type.clone(), |ctx| {
                    type_check_impl(ctx, binding_ctx, body)
                })
            } else {
                (
                    Type::bottom(),
                    vec![TypeError::UnguardedFixpoint(*var, term.span)],
                )
            }
        }
    }
}

pub fn type_check<'src, 'a>(
    binding_ctx: &BindingContext<'src, 'a>,
    term: &'a WithSpan<'src, Term<'src, 'a>>,
    name: Symbol<'src>,
) -> Vec<TypeError<'src>> {
    let mut typing_ctx = TypeContext::new();
    let mut proxy = BindingProxy::proxy(binding_ctx);
    proxy.with_hiding(name, |binding_ctx| {
        type_check_impl(&mut typing_ctx, binding_ctx, term).1
    })
}
