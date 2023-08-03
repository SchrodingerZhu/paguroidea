// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::congruence::{approximate_congruence_class, meet};
use crate::derivative::derivative;
use crate::intervals::Intervals;
use crate::normalization::normalize;
use crate::regex_tree::RegexTree;
use crate::utilities::{self, dbg_sort};

use crate::lookahead::LoopOptimizer;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::io;
use std::rc::Rc;

#[derive(Hash, PartialEq, Eq, Debug, Clone, Ord, PartialOrd)]
pub struct Vector {
    regex_trees: Vec<Rc<RegexTree>>,
}

impl Display for Vector {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, regex_tree) in self.regex_trees.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", regex_tree)?;
        }
        write!(f, ")")
    }
}

pub struct DFAOptions {
    pub static_success: bool,
    pub unicode: bool,
    pub lookahead: bool,
    pub simd_limit: usize,
}

impl Default for DFAOptions {
    fn default() -> Self {
        Self {
            static_success: true,
            unicode: false,
            lookahead: true,
            simd_limit: 4,
        }
    }
}

impl Vector {
    pub fn new<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Rc<RegexTree>>,
    {
        let regex_trees = iter.into_iter().collect();
        Self { regex_trees }
    }

    pub fn is_byte_sequence(&self) -> bool {
        let mut iter = self
            .regex_trees
            .iter()
            .filter(|x| !matches!(x.as_ref(), RegexTree::Bottom))
            .map(|x| x.is_byte_sequence());
        matches!(iter.next(), Some(true)) && iter.next().is_none()
    }

    pub fn as_byte_sequence(&self) -> Option<(usize, Vec<u8>)> {
        let failing = self
            .regex_trees
            .iter()
            .filter(|x| matches!(x.as_ref(), RegexTree::Bottom))
            .count();
        if failing == self.regex_trees.len() - 1 {
            self.regex_trees
                .iter()
                .enumerate()
                .find_map(|(idx, x)| x.as_byte_sequence().map(|x| (idx, x)))
        } else {
            None
        }
    }

    pub fn derivative(&self, x: u8) -> Self {
        Vector {
            regex_trees: self
                .regex_trees
                .iter()
                .map(|t| derivative(t.clone(), x))
                .collect(),
        }
    }

    pub fn accepting_state(&self) -> Option<usize> {
        self.regex_trees.iter().enumerate().find_map(|t| {
            if t.1.is_nullable() {
                Some(t.0)
            } else {
                None
            }
        })
    }

    pub fn is_rejecting_state(&self) -> bool {
        self.regex_trees
            .iter()
            .all(|t| matches!(t.as_ref(), RegexTree::Bottom))
    }

    pub fn approximate_congruence_class(&self) -> Vec<Intervals> {
        // meet all congruence classes for each regex tree
        self.regex_trees
            .iter()
            .map(|x| approximate_congruence_class(x))
            .reduce(|acc, x| meet(acc.as_slice(), x.as_slice()))
            .unwrap_or_default()
    }

    pub fn normalize(&self) -> Self {
        let regex_trees = self
            .regex_trees
            .iter()
            .map(|x| normalize(x.clone()))
            .collect();
        Self { regex_trees }
    }

    pub fn generate_dfa(
        &self,
        initial_idx: &TokenStream,
        optimizer: &mut LoopOptimizer,
        success_actions: &[TokenStream],
        failure_action: &TokenStream,
        options: &DFAOptions,
    ) -> TokenStream {
        let initial_state = {
            let initial_state = self.normalize();
            let last_success = LastSuccess::relevant(initial_state.accepting_state());
            DfaState {
                state_vec: initial_state,
                last_success,
                unicode: None,
            }
        };
        let mut dfa = build_dfa(initial_state.state_vec.clone(), options);
        let leaf_states = extract_leaf_states(&mut dfa);
        let initial_label = format_ident!("S{}", dfa[&initial_state].state_id);
        let dispatch_last_success = if options.static_success {
            todo!()
        } else {
            None
        };
        let actions = dbg_sort(&dfa, |(_, info)| info.state_id).map(|(state, info)| {
            let label = format_ident!("S{}", info.state_id);
            if let Some((rule_idx, seq)) = state.state_vec.as_byte_sequence() {
                let literal = Literal::byte_string(&seq);
                let length = seq.len();
                let on_success = &success_actions[rule_idx];
                return quote! {
                    State::#label => {
                        unsafe { ::pag_util::assume(idx <= input.len()) };
                        if input[idx..].starts_with(#literal) {
                            cursor = idx + #length;
                            #on_success
                        } else {
                            #failure_action
                        }
                    },
                };
            }
            let lookahead = optimizer.generate_lookahead(&dfa, state);
            let transitions = info.transitions.iter().map(|(interval, target)| {
                if leaf_states.contains(target) {
                    let rule_idx = target.last_success.unwrap();
                    let on_success = &success_actions[rule_idx];
                    return quote! { Some(#interval) => { cursor = idx + 1; #on_success }, };
                }
                let target_id = dfa[target].state_id;
                #[cfg(not(target_arch = "aarch64"))]
                if lookahead.is_some() && info.state_id == target_id {
                    return quote! {};
                }
                let target_label = format_ident!("S{}", target_id);
                quote! { Some(#interval) => state = State::#target_label, }
            });
            let otherwise = state
                .last_success
                .and_then(|x| success_actions.get(x))
                .unwrap_or(failure_action);
            let advance_cursor = if state.state_vec.accepting_state().is_some() {
                Some(quote!(cursor = idx;))
            } else {
                None
            };
            quote! {
                State::#label => {
                    #lookahead
                    #advance_cursor
                    match input.get(idx) {
                        #(#transitions)*
                        _ => { #otherwise }
                    }
                },
            }
        });

        let labels = dbg_sort(dfa.values(), |info| info.state_id)
            .map(|info| format_ident!("S{}", info.state_id));

        quote! {
            enum State {
                #(#labels,)*
            }
            let mut idx = #initial_idx;
            let mut state = State::#initial_label;
            loop {
                match state {
                    #(#actions)*
                }
                idx += 1;
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum LastSuccess {
    Irrelevant,
    Success(usize),
    None,
}

impl LastSuccess {
    pub fn update(self, current_success: Option<usize>) -> Self {
        match (self, current_success) {
            (LastSuccess::Irrelevant, _) => LastSuccess::Irrelevant,
            (_, None) => self,
            (LastSuccess::None, Some(x)) => LastSuccess::Success(x),
            (LastSuccess::Success(_), Some(x)) => LastSuccess::Success(x),
        }
    }
    pub fn relevant(success: Option<usize>) -> Self {
        match success {
            None => LastSuccess::None,
            Some(x) => LastSuccess::Success(x),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DfaState {
    state_vec: Vector,
    last_success: LastSuccess,
    unicode: Option<Rc<RegexTree>>,
}

#[derive(Debug, Clone)]
pub struct DfaInfo {
    state_id: usize,
    pub(crate) transitions: Vec<(Intervals, DfaState)>,
}

pub type DfaTable = HashMap<DfaState, DfaInfo>;

fn explore_dfa_node(dfa: &mut DfaTable, state: DfaState, state_id: &mut usize) {
    dfa.insert(
        state.clone(),
        DfaInfo {
            state_id: *state_id,
            transitions: vec![],
        },
    );
    *state_id += 1;
    if let Some(unicode) = state.unicode.as_ref() {
        if unicode.is_nullable() {
            if let Some((_, seq)) = state.state_vec.as_byte_sequence() {
                if std::str::from_utf8(&seq).is_ok() {
                    return;
                }
            }
        }
    } else if state.state_vec.is_byte_sequence() {
        return;
    }

    let classes = state.state_vec.approximate_congruence_class();
    let mut transitions = Vec::with_capacity(classes.len());

    for intervals in classes {
        let char = intervals.representative();
        let target = state.state_vec.derivative(char).normalize();
        let unicode = state
            .unicode
            .as_ref()
            .cloned()
            .map(|x| derivative(x, char))
            .map(normalize);
        let last_success = state.last_success.update(target.accepting_state());
        let next = DfaState {
            state_vec: target,
            last_success,
            unicode,
        };
        if !next.state_vec.is_rejecting_state()
            && next
                .unicode
                .map(|x| !matches!(&*x, RegexTree::Bottom))
                .unwrap_or(true)
        {
            transitions.push((intervals, next.clone()));
            if !dfa.contains_key(&next) {
                explore_dfa_node(dfa, next, state_id)
            }
        }
    }

    dfa.get_mut(&state).unwrap().transitions = transitions;
}

pub fn build_dfa(state: Vector, options: &DFAOptions) -> DfaTable {
    let mut state_id = 0;
    let mut dfa = HashMap::new();
    let last_success = if options.static_success {
        LastSuccess::relevant(state.accepting_state())
    } else {
        LastSuccess::Irrelevant
    };
    let unicode = if options.unicode {
        Some(utilities::unicode_codepoints())
    } else {
        None
    };
    let state = DfaState {
        state_vec: state,
        last_success,
        unicode,
    };
    explore_dfa_node(&mut dfa, state, &mut state_id);
    #[cfg(pag_print_dfa)]
    print_dfa(&dfa);
    dfa
}

fn extract_leaf_states(dfa: &mut DfaTable) -> HashSet<DfaState> {
    // TODO: switch to `drain_filter` (nightly) / `extract_if` (hashbrown)
    let leaf_states = dfa
        .iter()
        .filter_map(|(state, info)| {
            if info.transitions.is_empty() && state.last_success.is_some() {
                Some(state.clone())
            } else {
                None
            }
        })
        .collect();
    for s in &leaf_states {
        dfa.remove(s);
    }
    leaf_states
}

#[cfg(pag_print_dfa)]
fn print_dfa(dfa: &DfaTable) {
    for (state, info) in dfa {
        println!(
            "S{}({:?}): {}",
            info.state_id, state.last_success, state.state_vec
        );
        for (intervals, target) in &info.transitions {
            println!("  {} -> S{}", intervals, dfa[target].state_id);
        }
    }
}
