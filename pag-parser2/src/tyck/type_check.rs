use super::{TypeContext, Uid};
use crate::{
    frontend::{
        ParserDef,
        ParserExpr::{self, *},
        SpanBox,
        LexerDef,
    },
    tyck::BindingProxy,
};
use proc_macro2::Span;
use std::collections::{HashMap, HashSet};
use syn::{Error, Ident};

// #[derive(Debug)]
// pub enum TypeError {
//     SequentialUniquenessViolation {
//         // lhs: (Span<'a>, Type<'a>),
//         // rhs: (Span<'a>, Type<'a>),
//         // total: Span<'a>,
//     },
//     DisjunctiveUniquenessViolation {
//         // lhs: (Span<'a>, Type<'a>),
//         // rhs: (Span<'a>, Type<'a>),
//         // total: Span<'a>,
//     },
//     // UnguardedFixpoint(Symbol<'a>, Span<'a>),
//     // UnresolvedReference(Symbol<'a>, Span<'a>),
//     UnguardedFixpoint(Ident),
//     UnresolvedReference(Ident),
// }

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Type {
    pub first: HashSet<Ident>,
    pub follow: HashSet<Ident>,
    pub nullable: bool,
    pub guarded: bool,
}

impl Type {
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
    fn token(token: syn::Ident) -> Self {
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
        lhs: Span,
        rhs: Span,
        // total: Span<'src>,
    ) -> Result<Self, Box<Error>> {
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
            Err(Box::new(Error::new(
                lhs.join(rhs).unwrap(),
                "sequence error",
            )))
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
        lhs: Span,
        rhs: Span,
        // total: Span<'src>,
    ) -> Result<Self, Box<Error>> {
        if t1.disjunctive_uniqueness(t2) {
            Ok(Self {
                first: t1.first.union(&t2.first).cloned().collect(),
                follow: t1.follow.union(&t2.follow).cloned().collect(),
                nullable: t1.nullable || t2.nullable,
                guarded: t1.guarded && t2.guarded,
            })
        } else {
            Err(Box::new(Error::new(
                lhs.join(rhs).unwrap(),
                "alternative error",
            )))
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

    fn fixpoint<F>(mut f: F) -> (Self, Vec<Error>)
    where
        F: FnMut(&Self) -> (Self, Vec<Error>),
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

fn tyck_ident(
    typing_ctx: &mut TypeContext,
    binding_ctx: &mut BindingProxy,
    lexer_map: &HashMap<syn::Ident, LexerDef>,
    term: Ident,
) -> (Type, Vec<Error>) {
    // if ParserDef
    let parser_ctx = binding_ctx.binding;
    let rules = &parser_ctx.get(&term).unwrap().rules;

    let triple = rules
        .iter()
        .map(|rule| {
            // rule: ParserRule -> rule_triple: (Type, Vec<Error>, Span)
            rule.vars.iter().fold(
                (Type::minimum(), vec![], Span::call_site()),
                |var_res, var| {
                    let res = tyck_parserexpr(typing_ctx, binding_ctx, lexer_map, &var.expr);
                    let (r#type, err) = match Type::sequence(&var_res.0, &res.0, var_res.2, res.2) {
                        Ok(r#type) => (r#type, None),
                        Err(err) => (Type::bottom(), Some(err)),
                    };
                    (
                        r#type,
                        var_res 
                            .1
                            .into_iter()
                            .chain(res.1)
                            .chain(err.map(|e| *e))
                            .collect(),
                        var_res.2.join(res.2).unwrap()

                    )
                },
            )
        })
        .fold(
            (Type::minimum(), vec![], Span::call_site()),
            |rule_res, rule_triple| {
                let (r#type, err) =
                    match Type::alternative(&rule_res.0, &rule_triple.0, rule_res.2, rule_triple.2) {
                        Ok(r#type) => (r#type, None),
                        Err(err) => (Type::bottom(), Some(err)),
                    };
                (
                    r#type,
                    rule_res
                        .1
                        .into_iter()
                        .chain(rule_triple.1)
                        .chain(err.map(|e| *e))
                        .collect(),
                    rule_res.2.join(rule_triple.2).unwrap(),
                )
            },
        );

        (triple.0, triple.1)
   
}

fn tyck_parserexpr(
    typing_ctx: &mut TypeContext,
    binding_ctx: &mut BindingProxy,
    lexer_map: &HashMap<syn::Ident, LexerDef>,
    term: &SpanBox<ParserExpr>,
) -> (Type, Vec<Error>, Span) {
    match &term.node {
        Seq(x, y) => {

            let (x_type, x_errors, x_span) = tyck_parserexpr(typing_ctx, binding_ctx, lexer_map, &x);
            let (y_type, y_errors, y_span) = typing_ctx.guarded(|ctx| tyck_parserexpr(ctx, binding_ctx, lexer_map, &y));
            let (r#type, err) = match Type::sequence(&x_type, &y_type, x_span, y_span) {
                Ok(r#type) => (r#type, None),
                Err(err) => (Type::bottom(), Some(err)),
            };
            (
                r#type,
                x_errors
                    .into_iter()
                    .chain(y_errors)
                    .chain(err.map(|e| *e))
                    .collect(),
                x_span.join(y_span).unwrap(),
            )
        }
        // star = fix | empty
        Star(x) => {
            // \x . (i ~ x) | epsilon

            //let sequence = Seq(*x, Box::new(*term));
            //let body = spanned(Term::Alternative(sequence, spanned(Term::Epsilon)));
            // TODO: Check Correctness
            if let Some(ty) = typing_ctx.lookup(Uid::Pt(&term.node)) {
                return (ty.as_ref().clone(), vec![], term.span);
            }

            let (r#type, errs) = Type::fixpoint(|ty| {
                typing_ctx.with(Uid::Pt(&term.node), ty.clone(), |ctx| { 
                    //let seq = tyck_parserexpr(typing_ctx, binding_ctx, Box::new( WithSpan{span: x.span.join(term.span).unwrap(), node: Seq(x, term)}  ));
                    let (x_type, x_errors, x_span) = tyck_parserexpr(ctx, binding_ctx, lexer_map, &x);
                    let (y_type, y_errors, y_span) = tyck_parserexpr(ctx, binding_ctx, lexer_map, term);
                    let (l_type, l_err, l_span) = match Type::sequence(&x_type, &y_type, x_span, y_span) {
                        Ok(r#type) => (r#type, None, x_span.join(y_span).unwrap()),
                        Err(err) => (Type::bottom(), Some(err), x_span.join(y_span).unwrap()),
                    };
                    let (r_type, r_err, r_span) = (Type::epsilon(), vec![], Span::call_site());
                    let (r#type, err) = match Type::alternative(&l_type, &r_type, l_span, r_span) {
                        Ok(r#type) => (r#type, None),
                        Err(err) => (Type::bottom(), Some(err)),
                    };
                    (
                        r#type,
                        x_errors
                            .into_iter()
                            .chain(y_errors)
                            .chain(l_err.map(|e| *e))
                            .chain(r_err)
                            .chain(err.map(|e| *e))
                            .collect(),
                    )
                })
            });

            if !errs.is_empty() {
                return (r#type, errs, term.span);
            }
            if r#type.guarded {
                typing_ctx.with(Uid::Pt(&term.node), r#type.clone(), |ctx| {
                    let (x_type, x_err, x_span) = tyck_parserexpr(ctx, binding_ctx, lexer_map, &x);
                    let (y_type, y_err, y_span) = tyck_parserexpr(ctx, binding_ctx, lexer_map, term);
                    let (l_type, l_err, l_span) = match Type::sequence(&x_type, &y_type, x_span, y_span) {
                        Ok(r#type) => (r#type, None, x_span.join(y_span).unwrap()),
                        Err(err) => (Type::bottom(), Some(err), x_span.join(y_span).unwrap()),
                    };
                    let (r_type, r_err, r_span) = (Type::epsilon(), vec![], Span::call_site());
                    let (r#type, err) = match Type::alternative(&l_type, &r_type, l_span, r_span) {
                        Ok(r#type) => (r#type, None),
                        Err(err) => (Type::bottom(), Some(err)),
                    };
                    (
                        r#type,
                        x_err
                            .into_iter()
                            .chain(y_err)
                            .chain(l_err.map(|e| *e))
                            .chain(r_err)
                            .chain(err.map(|e| *e))
                            .collect(),
                        term.span
                    )
                })
            } else {
                (
                    Type::bottom(),
                    vec![Error::new(term.span, "unreachable fixpoint")],
                    term.span,
                )
            }
        }
        // plus = \x . i ~ ( eps | x )
        Plus(x) => {
            if let Some(ty) = typing_ctx.lookup(Uid::Pt(&term.node)) {
                return (ty.as_ref().clone(), vec![], term.span);
            }

            let (r#type, errs) = Type::fixpoint(|ty| {
                typing_ctx.with(Uid::Pt(&term.node), ty.clone(), |ctx| { 
                    //let seq = tyck_parserexpr(typing_ctx, binding_ctx, Box::new( WithSpan{span: x.span.join(term.span).unwrap(), node: Seq(x, term)}  ));

                    let (x_type, x_err, x_span) = tyck_parserexpr(ctx, binding_ctx, lexer_map, &x);
                    let (y_type, y_err, y_span) = (Type::epsilon(), vec![], Span::call_site());
                    let (r_type, r_err, r_span) = match Type::alternative(&x_type, &y_type, x_span, y_span) {
                        Ok(r#type) => (r#type, None, x_span.join(y_span).unwrap()),
                        Err(err) => (Type::bottom(), Some(err), x_span.join(y_span).unwrap()),
                    };

                    let (l_type, l_err, l_span) = tyck_parserexpr(ctx, binding_ctx, lexer_map, x);
                    let (r#type, err) = match Type::sequence(&l_type, &r_type, l_span, r_span) {
                        Ok(r#type) => (r#type, None),
                        Err(err) => (Type::bottom(), Some(err)),
                    };
                    (
                        r#type,
                        x_err
                            .into_iter()
                            .chain(y_err)
                            .chain(l_err)
                            .chain(r_err.map(|e| *e))
                            .chain(err.map(|e| *e))
                            .collect(),
                    )
                })
            });

            if !errs.is_empty() {
                return (r#type, errs, term.span);
            }

            if r#type.guarded {
                typing_ctx.with(Uid::Pt(&term.node), r#type.clone(), |ctx| {
                    let (x_type, x_err, x_span) = tyck_parserexpr(ctx, binding_ctx, lexer_map, &x);
                    let (y_type, y_err, y_span) = (Type::epsilon(), vec![], Span::call_site());
                    let (r_type, r_err, r_span) = match Type::alternative(&x_type, &y_type, x_span, y_span) {
                        Ok(r#type) => (r#type, None, x_span.join(y_span).unwrap()),
                        Err(err) => (Type::bottom(), Some(err), x_span.join(y_span).unwrap()),
                    };

                    let (l_type, l_err, l_span) = tyck_parserexpr(ctx, binding_ctx, lexer_map, x);
                    let (r#type, err) = match Type::sequence(&l_type, &r_type, l_span, r_span) {
                        Ok(r#type) => (r#type, None),
                        Err(err) => (Type::bottom(), Some(err)),
                    };
                    (
                        r#type,
                        x_err
                            .into_iter()
                            .chain(y_err)
                            .chain(l_err)
                            .chain(r_err.map(|e| *e))
                            .chain(err.map(|e| *e))
                            .collect(),
                            term.span
                    ) 
                })
            } else {
                (
                    Type::bottom(),
                    vec![Error::new(term.span, "unreachable fixpoint")],
                    term.span,
                )
            }
            
        }

        Opt(x) => {
            let (x_type, x_errors, x_span) = tyck_parserexpr(typing_ctx, binding_ctx, lexer_map, &x);
            let (r#type, err) = match Type::alternative(&x_type, &Type::epsilon(), x_span, Span::call_site()) {
                Ok(r#type) => (r#type, None),
                Err(err) => (Type::bottom(), Some(err)),
            };
            (
                r#type,
                x_errors
                    .into_iter()
                    .chain(err.map(|e| *e))
                    .collect(),
                term.span
            )
        }

        LexerRef(name) => {
            let lexer = lexer_map.get(&name);
            match lexer {
                Some(_) => (Type::token(name.clone()), vec![], term.span),
                None => (Type::bottom(), vec![Error::new(term.span, "unresolved lexer reference")], term.span),
            }
        }

        ParserRef(name) => {
            if let Some(ty) = typing_ctx.lookup(Uid::Id(name.clone())) {
                return (ty.as_ref().clone(), vec![], term.span);
            }
            // otherwise, we need to type check the parser definition.
            if binding_ctx.lookup(&name.clone()).is_some() {
                // we should not cache the result, since it can be recursive and changed during the calculation of the fixpoint.
                let (r#type, errors) = binding_ctx.with_hiding(name.clone(), |binding_ctx| {
                    tyck_ident(typing_ctx, binding_ctx, lexer_map, name.clone())
                });
                (r#type, errors, term.span)
            } else {
                (
                    Type::bottom(),
                    vec![Error::new(term.span, "unresolved parser reference")],
                    term.span,
                )
            }
        }

        Ignore(x) => {
            let (x_type, x_errors, x_span) = tyck_parserexpr(typing_ctx, binding_ctx, lexer_map, &x);
            (x_type, x_errors, x_span)
        }

    }
}

pub fn type_check(name: Ident, binding_ctx: &HashMap<syn::Ident, ParserDef>, lexer_map: &HashMap<syn::Ident, LexerDef>) -> Vec<Error> {
    let mut typing_ctx = TypeContext::new();
    let mut proxy = BindingProxy::proxy(binding_ctx);
    proxy.with_hiding(name.clone(), |binding_ctx| {
        tyck_ident(&mut typing_ctx, binding_ctx, lexer_map, name).1
    })
}
