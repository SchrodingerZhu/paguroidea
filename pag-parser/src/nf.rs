// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    num::NonZeroUsize,
    write,
};

use smallvec::{smallvec, SmallVec};
use typed_arena::Arena;

use crate::{core_syntax::Term, frontend::syntax::Parser, utilities::Symbol};

// thinking a while...

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tag<'src> {
    symbol: Symbol<'src>,
    version: Option<NonZeroUsize>,
}

impl<'src> Tag<'src> {
    pub fn new(symbol: Symbol<'src>) -> Self {
        Self {
            symbol,
            version: None,
        }
    }

    pub fn is_versioned(&self) -> bool {
        self.version.is_some()
    }

    pub fn symbol(&self) -> Symbol<'src> {
        self.symbol
    }
}

pub struct TagAssigner<'src> {
    counters: HashMap<Symbol<'src>, NonZeroUsize>,
}

impl<'src> TagAssigner<'src> {
    pub fn new() -> Self {
        Self {
            counters: HashMap::new(),
        }
    }

    fn next(&mut self, symbol: Symbol<'src>) -> Tag<'src> {
        let version = *self
            .counters
            .entry(symbol)
            .and_modify(|x| *x = x.saturating_add(1))
            .or_insert(NonZeroUsize::MIN);
        Tag {
            symbol,
            version: Some(version),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Action<'src> {
    Subroutine(Tag<'src>),
    Summarize(Symbol<'src>),
}

impl<'src> Display for Action<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Subroutine(tag) => write!(f, "{}", tag),
            Action::Summarize(tag) => write!(f, "[{}]", tag),
        }
    }
}

impl<'src> Action<'src> {
    fn symbol(&self) -> Symbol<'src> {
        match self {
            Action::Subroutine(tag) => tag.symbol,
            Action::Summarize(sym) => *sym,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NormalForm<'src> {
    Empty(SmallVec<[Symbol<'src>; 1]>),
    Unexpanded(SmallVec<[Action<'src>; 1]>),
    Sequence {
        terminal: Symbol<'src>,
        nonterminals: SmallVec<[Action<'src>; 1]>,
    },
}

impl<'src> Display for Tag<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.symbol.fmt(f)?;
        if let Some(x) = self.version {
            write!(f, "_{x}")?;
        }
        Ok(())
    }
}

impl<'src> Display for NormalForm<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NormalForm::Empty(trees) => trees.iter().fold(write!(f, "ε"), |acc, x| {
                acc.and_then(|_| write!(f, " [{x}]"))
            }),
            NormalForm::Sequence {
                terminal,
                nonterminals,
            } => {
                write!(f, "{terminal}")?;
                for i in nonterminals {
                    write!(f, " {i}")?;
                }
                Ok(())
            }
            NormalForm::Unexpanded(tags) => {
                write!(f, "{}", tags[0])?;
                for i in &tags[1..] {
                    write!(f, " {i}")?;
                }
                Ok(())
            }
        }
    }
}

pub struct NormalForms<'src, 'a> {
    pub entries: HashMap<Tag<'src>, SmallVec<[&'a NormalForm<'src>; 4]>>,
}

impl<'src, 'a> NormalForms<'src, 'a> {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
}

impl<'src, 'a> Display for NormalForms<'src, 'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (tag, nf) in self.entries.iter() {
            for i in nf {
                writeln!(f, "{tag} -> {i}")?;
            }
        }
        Ok(())
    }
}

pub fn semi_normalize<'src, 'p, 'nf>(
    target: &Term<'src, 'p>,
    tag: Tag<'src>,
    arena: &'nf Arena<NormalForm<'src>>,
    nfs: &mut NormalForms<'src, 'nf>,
    assigner: &mut TagAssigner<'src>,
    parser: &Parser<'src, 'p>,
) {
    let ret_tag = semi_normalize_helper(target, tag, arena, nfs, assigner, parser);
    nfs.entries
        .entry(tag)
        .or_insert(smallvec![&*arena.alloc(NormalForm::Unexpanded(smallvec![
            Action::Subroutine(ret_tag)
        ]))]);
}

pub fn semi_normalize_helper<'src, 'p, 'nf>(
    target: &Term<'src, 'p>,
    tag: Tag<'src>,
    arena: &'nf Arena<NormalForm<'src>>,
    nfs: &mut NormalForms<'src, 'nf>,
    assigner: &mut TagAssigner<'src>,
    parser: &Parser<'src, 'p>,
) -> Tag<'src> {
    match target {
        Term::Epsilon => {
            let nf = smallvec![&*arena.alloc(NormalForm::Empty(Default::default()))];
            nfs.entries.insert(tag, nf);
            tag
        }
        Term::Sequence(x, y) => {
            let mut x_tag = assigner.next(tag.symbol);
            let mut y_tag = assigner.next(tag.symbol);
            x_tag = semi_normalize_helper(&x.node, x_tag, arena, nfs, assigner, parser);
            y_tag = semi_normalize_helper(&y.node, y_tag, arena, nfs, assigner, parser);
            let acts = smallvec![Action::Subroutine(x_tag), Action::Subroutine(y_tag)];
            let nf = smallvec![&*arena.alloc(NormalForm::Unexpanded(acts))];
            nfs.entries.insert(tag, nf);
            tag
        }
        Term::LexerRef(lexer) => {
            let nf = smallvec![&*arena.alloc(NormalForm::Sequence {
                terminal: *lexer,
                nonterminals: SmallVec::new(),
            })];
            nfs.entries.insert(tag, nf);
            tag
        }
        Term::Bottom => {
            let nf = SmallVec::new();
            nfs.entries.insert(tag, nf);
            tag
        }
        Term::Alternative(x, y) => {
            let mut x_tag = assigner.next(tag.symbol);
            let mut y_tag = assigner.next(tag.symbol);
            x_tag = semi_normalize_helper(&x.node, x_tag, arena, nfs, assigner, parser);
            y_tag = semi_normalize_helper(&y.node, y_tag, arena, nfs, assigner, parser);
            let nf = smallvec![
                &*arena.alloc(NormalForm::Unexpanded(smallvec![Action::Subroutine(x_tag)])),
                &*arena.alloc(NormalForm::Unexpanded(smallvec![Action::Subroutine(y_tag)])),
            ];
            nfs.entries.insert(tag, nf);
            tag
        }
        Term::Fix(var, body) => {
            let body_tag = Tag::new(*var);
            semi_normalize_helper(&body.node, body_tag, arena, nfs, assigner, parser);
            // copy tag for fixpoint
            if tag != body_tag {
                let body_nf = nfs.entries.get(&body_tag).unwrap();
                nfs.entries.insert(tag, body_nf.clone());
            }
            body_tag
        }
        Term::ParserRef(x) => {
            let ref_tag = Tag::new(*x);
            if parser.is_active(&ref_tag) {
                let acts = smallvec![Action::Subroutine(ref_tag), Action::Summarize(*x)];
                let nf = smallvec![&*arena.alloc(NormalForm::Unexpanded(acts))];
                nfs.entries.insert(tag, nf);
                tag
            } else {
                ref_tag
            }
        }
    }
}

pub fn fully_normalize<'src, 'nf>(
    arena: &'nf Arena<NormalForm<'src>>,
    nfs: &mut NormalForms<'src, 'nf>,
) {
    let mut updates = Vec::new();
    loop {
        for (tag, i) in nfs.entries.iter() {
            if !i.iter().any(|x| matches!(x, NormalForm::Unexpanded(..))) {
                continue;
            }
            let mut result = SmallVec::new();
            for j in i.iter() {
                let NormalForm::Unexpanded(actions) = j else {
                    result.push(*j);
                    continue;
                };
                let first_subroutine = actions.iter().enumerate().find_map(|(index, act)| {
                    if let Action::Subroutine(x) = act {
                        Some((index, x))
                    } else {
                        None
                    }
                });
                match first_subroutine {
                    None => {
                        let nf = NormalForm::Empty(actions.iter().map(|x| x.symbol()).collect());
                        result.push(&*arena.alloc(nf));
                    }
                    Some((index, x)) => {
                        let variable_nf = nfs.entries.get(x).unwrap();
                        for k in variable_nf.iter().copied() {
                            let head = actions[..index].iter().cloned();
                            let tail = actions[index + 1..].iter().cloned();
                            match k {
                                NormalForm::Empty(trees) => {
                                    let insert = trees.iter().map(|x| Action::Summarize(*x));
                                    let acts = head.chain(insert).chain(tail).collect();
                                    result.push(&*arena.alloc(NormalForm::Unexpanded(acts)));
                                }
                                NormalForm::Unexpanded(subacts) => {
                                    let insert = subacts.iter().cloned();
                                    let acts = head.chain(insert).chain(tail).collect();
                                    result.push(&*arena.alloc(NormalForm::Unexpanded(acts)));
                                }
                                NormalForm::Sequence {
                                    terminal,
                                    nonterminals,
                                } => {
                                    let insert = nonterminals.iter().cloned();
                                    let acts = head.chain(insert).chain(tail).collect();
                                    result.push(&*arena.alloc(NormalForm::Sequence {
                                        terminal: *terminal,
                                        nonterminals: acts,
                                    }));
                                }
                            }
                        }
                    }
                }
            }
            updates.push((*tag, result));
        }
        if updates.is_empty() {
            break;
        }
        nfs.entries.extend(updates.drain(..));
    }
}

pub fn merge_inactive_rules<'src, 'nf>(
    nfs: &mut NormalForms<'src, 'nf>,
    parser: &Parser<'src, '_>,
    arena: &'nf Arena<NormalForm<'src>>,
) {
    // sort all rules
    for i in nfs.entries.values_mut() {
        i.sort_unstable();
    }
    let mut table: HashMap<&[&NormalForm], Tag<'src>> = HashMap::new();
    let mut rename = Vec::new();
    for (tag, nf) in nfs.entries.iter() {
        if parser.is_active(tag) {
            continue;
        }
        table
            .entry(nf.as_slice())
            .and_modify(|new_tag| rename.push((*tag, *new_tag)))
            .or_insert(*tag);
    }
    for (tag, new_tag) in rename {
        nfs.entries.remove(&tag);
        for i in nfs.entries.values_mut() {
            for j in i.iter_mut() {
                let NormalForm::Sequence {
                    terminal,
                    nonterminals,
                } = j else { continue };
                if nonterminals.contains(&Action::Subroutine(tag)) {
                    *j = &*arena.alloc(NormalForm::Sequence {
                        terminal: *terminal,
                        nonterminals: nonterminals
                            .iter()
                            .map(|x| {
                                if *x == Action::Subroutine(tag) {
                                    Action::Subroutine(new_tag)
                                } else {
                                    *x
                                }
                            })
                            .collect(),
                    });
                }
            }
        }
    }
}

pub fn remove_unreachable_rules<'src>(nfs: &mut NormalForms<'src, '_>, parser: &Parser<'src, '_>) {
    fn dfs<'src>(
        nfs: &NormalForms<'src, '_>,
        current: Tag<'src>,
        visited: &mut HashSet<Tag<'src>>,
    ) {
        if visited.contains(&current) {
            return;
        }
        visited.insert(current);
        let Some(tag) = nfs.entries.get(&current) else { return };
        for i in tag {
            let NormalForm::Sequence { nonterminals, .. } = i else { continue };
            for i in nonterminals {
                let Action::Subroutine(x) = i else { continue };
                dfs(nfs, *x, visited);
            }
        }
    }

    let mut visited = HashSet::new();
    dfs(nfs, Tag::new(parser.entrypoint), &mut visited);
    nfs.entries.retain(|k, _| visited.contains(k));
}
