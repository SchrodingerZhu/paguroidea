/*
Copyright (C) 2023 Paguroidea Developpers

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at <https://mozilla.org/MPL/2.0/>.
*/

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Display,
    matches,
    num::NonZeroUsize,
    write,
};

use smallvec::SmallVec;
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
        use std::collections::hash_map::Entry;
        match self.counters.entry(symbol) {
            Entry::Occupied(mut x) => {
                let next = x.get().saturating_add(1);
                x.insert(next);
                Tag {
                    symbol,
                    version: Some(next),
                }
            }
            Entry::Vacant(x) => {
                x.insert(NonZeroUsize::MIN);
                Tag {
                    symbol,
                    version: Some(NonZeroUsize::MIN),
                }
            }
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
    Empty(SmallVec<[Symbol<'src>; 4]>),
    Unexpanded(SmallVec<[Action<'src>; 4]>),
    Sequence {
        terminal: Symbol<'src>,
        nonterminals: SmallVec<[Action<'src>; 4]>,
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
    match target {
        crate::core_syntax::Term::Epsilon => {
            let nf = [arena.alloc(NormalForm::Empty(Default::default())) as &'nf _]
                .into_iter()
                .collect();
            nfs.entries.insert(tag, nf);
        }
        crate::core_syntax::Term::Sequence(x, y) => {
            let x_tag = assigner.next(tag.symbol);
            let y_tag = assigner.next(tag.symbol);
            semi_normalize(&x.node, x_tag, arena, nfs, assigner, parser);
            semi_normalize(&y.node, y_tag, arena, nfs, assigner, parser);
            let result = [NormalForm::Unexpanded(
                [Action::Subroutine(x_tag), Action::Subroutine(y_tag)]
                    .into_iter()
                    .collect(),
            )]
            .into_iter()
            .map(|x| arena.alloc(x) as &'nf _)
            .collect();
            nfs.entries.insert(tag, result);
        }
        crate::core_syntax::Term::LexerRef(lexer) => {
            let nf = [arena.alloc(NormalForm::Sequence {
                terminal: *lexer,
                nonterminals: SmallVec::new(),
            }) as &'nf _]
            .into_iter()
            .collect();
            nfs.entries.insert(tag, nf);
        }
        crate::core_syntax::Term::Bottom => {
            let nf = SmallVec::new();
            nfs.entries.insert(tag, nf);
        }
        crate::core_syntax::Term::Alternative(x, y) => {
            let x_tag = assigner.next(tag.symbol);
            let y_tag = assigner.next(tag.symbol);
            semi_normalize(&x.node, x_tag, arena, nfs, assigner, parser);
            semi_normalize(&y.node, y_tag, arena, nfs, assigner, parser);
            let result = [
                NormalForm::Unexpanded([Action::Subroutine(x_tag)].into_iter().collect()),
                NormalForm::Unexpanded([Action::Subroutine(y_tag)].into_iter().collect()),
            ]
            .into_iter()
            .map(|x| arena.alloc(x) as &'nf _)
            .collect();
            nfs.entries.insert(tag, result);
        }
        crate::core_syntax::Term::Fix(var, body) => {
            let body_tag = Tag::new(*var);
            semi_normalize(&body.node, body_tag, arena, nfs, assigner, parser);
            // copy tag for fixpoint
            if tag != body_tag {
                let body_nf = nfs.entries.get(&body_tag).unwrap();
                nfs.entries.insert(tag, body_nf.clone());
            }
        }
        crate::core_syntax::Term::ParserRef(x) => {
            let nf = [arena.alloc(NormalForm::Unexpanded(
                [Action::Subroutine(Tag::new(*x))]
                    .into_iter()
                    .chain(if parser.is_active(&Tag::new(*x)) {
                        Some(Action::Summarize(*x))
                    } else {
                        None
                    })
                    .collect(),
            )) as &'nf _]
            .into_iter()
            .collect();
            nfs.entries.insert(tag, nf);
        }
    }
}

pub fn dfs_remove_unreachable_rules<'src>(
    nfs: &mut NormalForms<'src, '_>,
    parser: &Parser<'src, '_>,
) {
    let mut visited = HashSet::new();
    fn dfs<'src>(
        nfs: &NormalForms<'src, '_>,
        current: Tag<'src>,
        visited: &mut HashSet<Tag<'src>>,
    ) {
        if visited.contains(&current) {
            return;
        }
        visited.insert(current);
        if let Some(tag) = nfs.entries.get(&current) {
            for i in tag {
                match i {
                    NormalForm::Empty(_) => (),
                    NormalForm::Unexpanded(_) => (),
                    NormalForm::Sequence { nonterminals, .. } => {
                        for i in nonterminals {
                            if let Action::Subroutine(x) = i {
                                dfs(nfs, *x, visited);
                            }
                        }
                    }
                }
            }
        }
    }
    dfs(nfs, Tag::new(parser.entrypoint), &mut visited);
    let to_remove = nfs
        .entries
        .keys()
        .filter(|x| !visited.contains(x))
        .copied()
        .collect::<Vec<_>>();
    for i in to_remove {
        nfs.entries.remove(&i);
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
        if !tag.is_versioned()
            && parser
                .bindings
                .get(&tag.symbol)
                .map(|x| x.active)
                .unwrap_or(false)
        {
            continue;
        }
        match table.entry(nf.as_slice()) {
            Entry::Occupied(x) => {
                rename.push((*tag, *x.get()));
            }
            Entry::Vacant(x) => {
                x.insert(*tag);
            }
        }
    }
    for (tag, new_tag) in rename {
        nfs.entries.remove(&tag);
        for i in nfs.entries.values_mut() {
            for j in i.iter_mut() {
                match j {
                    NormalForm::Sequence {
                        terminal,
                        nonterminals,
                    } => {
                        if nonterminals.contains(&Action::Subroutine(tag)) {
                            *j = arena.alloc(NormalForm::Sequence {
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
                            }) as &'nf _;
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

pub fn fully_normalize<'src, 'nf>(
    arena: &'nf Arena<NormalForm<'src>>,
    nfs: &mut NormalForms<'src, 'nf>,
) {
    loop {
        let mut updates = Vec::new();
        for (tag, i) in nfs.entries.iter() {
            if !i.iter().any(|x| matches!(x, NormalForm::Unexpanded(..))) {
                continue;
            }
            let mut result = SmallVec::new();
            for j in i.iter() {
                match j {
                    NormalForm::Empty(..) | NormalForm::Sequence { .. } => result.push(*j),
                    NormalForm::Unexpanded(actions) => {
                        match actions
                            .iter()
                            .enumerate()
                            .find(|(_, x)| matches!(x, Action::Subroutine(..)))
                        {
                            None => {
                                let nf =
                                    NormalForm::Empty(actions.iter().map(|x| x.symbol()).collect());
                                result.push(arena.alloc(nf) as &'nf _);
                            }
                            Some((index, Action::Subroutine(x))) => {
                                let head = &actions[..index];
                                let tail = &actions[index + 1..];
                                let variable_nf = nfs.entries.get(x).unwrap();
                                for k in variable_nf.iter().copied() {
                                    match k {
                                        NormalForm::Empty(trees) => {
                                            let acts = head
                                                .iter()
                                                .cloned()
                                                .chain(trees.iter().map(|x| Action::Summarize(*x)))
                                                .chain(tail.iter().cloned())
                                                .collect();
                                            result
                                                .push(arena.alloc(NormalForm::Unexpanded(acts))
                                                    as &'nf _);
                                        }
                                        NormalForm::Unexpanded(subacts) => {
                                            let acts = head
                                                .iter()
                                                .chain(subacts.iter())
                                                .chain(tail.iter())
                                                .cloned()
                                                .collect();
                                            result
                                                .push(arena.alloc(NormalForm::Unexpanded(acts))
                                                    as &'nf _);
                                        }
                                        NormalForm::Sequence {
                                            terminal,
                                            nonterminals,
                                        } => {
                                            let acts = head
                                                .iter()
                                                .chain(nonterminals.iter())
                                                .chain(tail.iter())
                                                .cloned()
                                                .collect();
                                            result.push(arena.alloc(NormalForm::Sequence {
                                                terminal: *terminal,
                                                nonterminals: acts,
                                            })
                                                as &'nf _);
                                        }
                                    }
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
            updates.push((*tag, result));
        }
        if updates.is_empty() {
            break;
        } else {
            for (tag, i) in updates.into_iter() {
                nfs.entries.insert(tag, i);
            }
        }
    }
}
