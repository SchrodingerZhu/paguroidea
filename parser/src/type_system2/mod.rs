use crate::core_syntax::{BindingContext, Term};
use crate::frontend::WithSpan;
use crate::unreachable_branch;
use crate::utilities::Symbol;
use std::collections::HashSet;
use std::fmt::Debug;
use std::{todo, vec};

use crate::type_system2::context::TypeContext;
use pest::Span;
use thiserror::Error;

mod context;

#[derive(Error, Debug)]
pub enum TypeError<'a> {
    #[error("sequential uniqueness requirement volidation")]
    SequentialUniquenessViolation {
        lhs: Span<'a>,
        rhs: Span<'a>,
        total: Span<'a>,
    },
    #[error("disjunctive uniqueness requirement volidation")]
    DisjunctiveUniquenessViolation {
        lhs: Span<'a>,
        rhs: Span<'a>,
        total: Span<'a>,
    },
    #[error("unguarded fixpoint {0}")]
    UnguardedFixpoint(Symbol<'a>, Span<'a>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct Type<'src> {
    first: HashSet<Symbol<'src>>,
    follow: HashSet<Symbol<'src>>,
    nullable: bool,
    guarded: bool,
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
    ) -> Result<Self, TypeError<'src>> {
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
            Err(TypeError::SequentialUniquenessViolation { lhs, rhs, total })
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
    ) -> Result<Self, TypeError<'src>> {
        if t1.disjunctive_uniqueness(t2) {
            Ok(Self {
                first: t1.first.union(&t2.first).cloned().collect(),
                follow: t1.follow.union(&t2.follow).cloned().collect(),
                nullable: t1.nullable || t2.nullable,
                guarded: t1.guarded && t2.guarded,
            })
        } else {
            Err(TypeError::DisjunctiveUniquenessViolation { lhs, rhs, total })
        }
    }
    // fn star(t: &Self, inner: Span<'src>, outer: Span<'src>) -> Result<Self, TypeError<'src>> {
    //     if t.sequential_uniqueness(t) {
    //         Ok(Self {
    //             first: t.first.clone(),
    //             follow: t.first.union(&t.follow).cloned().collect(),
    //             nullable: true,
    //             guarded: t.guarded,
    //         })
    //     } else {
    //         Err(TypeError::SequentialUniquenessViolation {
    //             lhs: inner,
    //             rhs: inner,
    //             total: outer,
    //         })
    //     }
    // }
    fn minimum() -> Self {
        Self {
            first: HashSet::new(),
            follow: HashSet::new(),
            nullable: false,
            guarded: false,
        }
    }
    fn fixpoint<F>(mut f: F) -> Self
    where
        F: FnMut(&Self) -> Self,
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

fn type_check_impl<'src, 'a>(
    typing_ctx: &mut TypeContext<'src>,
    binding_ctx: &BindingContext<'src, 'a>,
    term: &'a WithSpan<'src, Term<'src, 'a>>,
) -> (Type<'src>, Vec<TypeError<'src>>) {
    match &term.node {
        Term::Epsilon => (Type::epsilon(), vec![]),
        Term::Sequence(x, y) => {
            let (x_type, x_errors) = type_check_impl(typing_ctx, binding_ctx, x);
            let (y_type, y_errors) = type_check_impl(typing_ctx, binding_ctx, y);
            let (r#type, err) = match Type::sequence(&x_type, &y_type, x.span, y.span, term.span) {
                Ok(r#type) => (r#type, None),
                Err(err) => (Type::bottom(), Some(err)),
            };
            (
                r#type,
                x_errors.into_iter().chain(y_errors).chain(err).collect(),
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
                x_errors.into_iter().chain(y_errors).chain(err).collect(),
            )
        }
        Term::ParserRef(name) => {
            // first check if name is already typed in the context.
            // if so return that type directly.
            if let Some(ty) = typing_ctx.lookup(*name) {
                return (ty.as_ref().clone(), vec![]);
            }
            // otherwise, we need to type check the parser definition.
            if let Some(target) = binding_ctx.get(name) {
                // we should not cache the result, since it can be recursive and changed during the calculation of the fixpoint.
                let (r#type, errors) = type_check_impl(typing_ctx, binding_ctx, target.term);
                (r#type, errors)
            } else {
                // unreachable because the parser definition should be in the context.
                // this should be garanteed when translating the Parser Tree to the AST.
                unreachable_branch();
            }
        }
        Term::Fix(var, body) => {
            let r#type = Type::fixpoint(|x| {
                typing_ctx
                    .with(var.clone(), x.clone(), |ctx| {
                        type_check_impl(ctx, binding_ctx, body)
                    })
                    .0
            });
            if r#type.guarded {
                typing_ctx.with(var.clone(), r#type.clone(), |ctx| {
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
) -> Vec<TypeError<'src>> {
    let mut typing_ctx = TypeContext::new();
    type_check_impl(&mut typing_ctx, binding_ctx, term).1
}
