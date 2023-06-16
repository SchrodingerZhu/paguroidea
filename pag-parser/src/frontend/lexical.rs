// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use pag_lexer::{normalization::normalize, regex_tree::RegexTree};
use pest::Span;

use crate::utilities::{merge_results, unreachable_branch, Symbol};

use super::{
    unicode::{encode_char, encode_range},
    FrontendError::*,
    FrontendResult,
    SurfaceSyntaxTree::{self, *},
    WithSpan,
};

type SpanRegexTree<'src> = WithSpan<'src, Rc<RegexTree>>;

pub struct LexerDatabase<'src> {
    pub symbol_set: HashSet<&'src str>,
    pub entries: HashMap<Symbol<'src>, SpanRegexTree<'src>>,
    pub skip: Option<SpanRegexTree<'src>>,
}

impl<'src> LexerDatabase<'src> {
    pub fn nullability_check(&self) -> FrontendResult<'src, ()> {
        let mut errs = Vec::new();
        for (sym, rule) in &self.entries {
            if rule.node.is_nullable() {
                errs.push(NullableToken(sym.name(), rule.span));
            }
        }
        if let Some(skip) = &self.skip {
            if skip.node.is_nullable() {
                errs.push(NullableToken("<skip>", skip.span));
            }
        }
        if !errs.is_empty() {
            return Err(errs);
        }
        Ok(())
    }
}

enum State<'src, 'local> {
    Unresolved(&'local WithSpan<'src, SurfaceSyntaxTree<'src>>),
    Pending,
    Resolved(Rc<RegexTree>),
}

pub fn construct_lexer_database<'src>(
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> FrontendResult<'src, LexerDatabase<'src>> {
    let LexerDef { rules } = &sst.node else {
        unreachable_branch!("sst should be a lexical definition")
    };

    let mut rule_defs = HashMap::new();
    let mut skip_def = None;
    let mut errs = Vec::new();
    for rule in rules {
        match &rule.node {
            LexicalRuleDef { name, expr } => {
                let value = (name.span, Cell::new(State::Unresolved(expr)));
                if let Some((previous, _)) = rule_defs.insert(name.span.as_str(), value) {
                    errs.push(MultipleDefinition(previous, name.span));
                }
            }
            LexicalSkipDef { expr } => {
                if let Some((previous, _)) = skip_def.replace((rule.span, expr)) {
                    errs.push(MultipleSkippingRule(previous, rule.span));
                }
            }
            _ => {}
        }
    }
    if !errs.is_empty() {
        return Err(errs);
    }

    let mut entries = HashMap::new();
    for (name, (span, state)) in &rule_defs {
        let node = match state.replace(State::Pending) {
            State::Unresolved(expr_sst) => {
                let expr_regex = construct_regex_tree(expr_sst, &rule_defs)?;
                let expr_regex = normalize(expr_regex);
                state.set(State::Resolved(expr_regex.clone()));
                expr_regex
            }
            State::Pending => unreachable!(),
            State::Resolved(expr_regex) => {
                state.set(State::Resolved(expr_regex.clone()));
                expr_regex
            }
        };
        entries.insert(Symbol::new(name), WithSpan { span: *span, node });
    }

    let mut skip = None;
    if let Some((span, skip_sst)) = skip_def {
        let node = construct_regex_tree(skip_sst, &rule_defs)?;
        let node = normalize(node);
        skip = Some(WithSpan { span, node });
    }

    Ok(LexerDatabase {
        entries,
        symbol_set: rule_defs.keys().copied().collect(),
        skip,
    })
}

fn construct_regex_tree<'src>(
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
    rule_defs: &HashMap<&'src str, (Span<'src>, Cell<State<'src, '_>>)>,
) -> FrontendResult<'src, Rc<RegexTree>> {
    match &sst.node {
        LexicalAlternative { lhs, rhs } => {
            let lhs = construct_regex_tree(lhs, rule_defs);
            let rhs = construct_regex_tree(rhs, rule_defs);
            merge_results(lhs, rhs, |l, r| Rc::new(RegexTree::Union(l, r)))
        }
        LexicalSequence { lhs, rhs } => {
            let lhs = construct_regex_tree(lhs, rule_defs);
            let rhs = construct_regex_tree(rhs, rule_defs);
            merge_results(lhs, rhs, |l, r| Rc::new(RegexTree::Concat(l, r)))
        }
        LexicalAnd { lhs, rhs } => {
            let lhs = construct_regex_tree(lhs, rule_defs);
            let rhs = construct_regex_tree(rhs, rule_defs);
            merge_results(lhs, rhs, |l, r| Rc::new(RegexTree::Intersection(l, r)))
        }
        LexicalStar { inner } => {
            let inner = construct_regex_tree(inner, rule_defs)?;
            Ok(Rc::new(RegexTree::KleeneClosure(inner)))
        }
        LexicalPlus { inner } => {
            let inner = construct_regex_tree(inner, rule_defs)?;
            Ok(Rc::new(RegexTree::Concat(
                inner.clone(),
                Rc::new(RegexTree::KleeneClosure(inner)),
            )))
        }
        LexicalOptional { inner } => {
            let inner = construct_regex_tree(inner, rule_defs)?;
            Ok(Rc::new(RegexTree::Union(
                inner,
                Rc::new(RegexTree::Epsilon),
            )))
        }
        LexicalNot { inner } => {
            let inner = construct_regex_tree(inner, rule_defs)?;
            Ok(Rc::new(RegexTree::Complement(inner)))
        }
        RangeLit { start, end } => Ok(encode_range(*start, *end)),
        StringLit(x) => Ok(x
            .bytes()
            .map(|b| Rc::new(RegexTree::single(b)))
            .reduce(|acc, b| Rc::new(RegexTree::Concat(acc, b)))
            .unwrap_or_else(|| Rc::new(RegexTree::Epsilon))),
        Bottom => Ok(Rc::new(RegexTree::Bottom)),
        Empty => Ok(Rc::new(RegexTree::Epsilon)),
        CharLit { value } => Ok(encode_char(value.node)),
        LexicalRuleRef { name } => match rule_defs.get(name.span.as_str()) {
            Some((_, state)) => match state.replace(State::Pending) {
                State::Unresolved(expr_sst) => {
                    let expr_regex = construct_regex_tree(expr_sst, rule_defs)?;
                    let expr_regex = normalize(expr_regex);
                    state.set(State::Resolved(expr_regex.clone()));
                    Ok(expr_regex)
                }
                State::Pending => Err(vec![CyclicLexicalRuleReference(name.span)]),
                State::Resolved(expr_regex) => {
                    state.set(State::Resolved(expr_regex.clone()));
                    Ok(expr_regex)
                }
            },
            None => Err(vec![UndefinedLexicalRuleReference(name.span)]),
        },
        _ => unreachable_branch!("called with unsupported node: {}", sst.span.as_str()),
    }
}
