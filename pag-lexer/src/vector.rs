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
use crate::unicode::UnicodeState;
use crate::utilities::dbg_sort;
use crate::DfaConfig;

use crate::lookahead::LoopOptimizer;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
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
        config: &DfaConfig,
    ) -> TokenStream {
        let initial_state = {
            let initial_state = self.normalize();
            let last_success = initial_state.accepting_state();
            DfaState {
                vector: initial_state,
                last_success,
                unicode: if config.unicode {
                    Some(UnicodeState::Accept)
                } else {
                    None
                },
            }
        };
        let mut dfa = build_dfa(initial_state.clone());
        let leaf_states = extract_leaf_states(&mut dfa, &initial_state);
        let initial_label = format_ident!("S{}", dfa[&initial_state].state_id);
        let actions = dbg_sort(&dfa, |(_, info)| info.state_id).map(|(state, info)| {
            let label = format_ident!("S{}", info.state_id);
            if let Some((rule_idx, seq)) = state.as_byte_sequence() {
                let literal = Literal::byte_string(&seq);
                let length = seq.len();
                let on_success = &success_actions[rule_idx];
                let on_failure = state
                    .last_success
                    .and_then(|x| success_actions.get(x))
                    .unwrap_or(failure_action);
                return quote! {
                    State::#label => {
                        if input[idx..].starts_with(#literal) {
                            cursor = idx + #length;
                            #on_success
                        } else {
                            #on_failure
                        }
                    },
                };
            }
            let lookahead = optimizer.generate_lookahead(&dfa, state, config);
            let transitions = info.transitions.iter().map(|(interval, target)| {
                if leaf_states.contains(target) {
                    if let Some((rule_idx, seq)) = target.as_byte_sequence() {
                        let literal = Literal::byte_string(&seq);
                        let length = seq.len();
                        let on_success = &success_actions[rule_idx];
                        let on_failure = target
                            .last_success
                            .and_then(|x| success_actions.get(x))
                            .unwrap_or(failure_action);
                        return quote! {
                            Some(#interval) => {
                                if input[idx + 1..].starts_with(#literal) {
                                    cursor = idx + 1 + #length;
                                    #on_success
                                } else {
                                    #on_failure
                                }
                            },
                        };
                    }
                    let action = state
                        .last_success
                        .and_then(|x| success_actions.get(x))
                        .unwrap_or(failure_action);
                    return quote! { Some(#interval) => { cursor = idx + 1; #action }, };
                }
                let target_id = dfa[target].state_id;
                if !cfg!(target_arch = "aarch64")
                    && lookahead.is_some()
                    && info.state_id == target_id
                {
                    return quote! {};
                }
                let target_label = format_ident!("S{}", target_id);
                quote! { Some(#interval) => state = State::#target_label, }
            });
            let otherwise = state
                .last_success
                .and_then(|x| success_actions.get(x))
                .unwrap_or(failure_action);
            let advance_cursor = if state.accepting_state().is_some() {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DfaState {
    vector: Vector,
    last_success: Option<usize>,
    unicode: Option<UnicodeState>,
}

impl DfaState {
    pub fn approximate_congruence_class(&self) -> Vec<Intervals> {
        match &self.unicode {
            None => self.vector.approximate_congruence_class(),
            Some(state) => {
                let classes = self.vector.approximate_congruence_class();
                let unicode_classes = state.congruence_classes();
                meet(&classes, &unicode_classes)
            }
        }
    }
    pub fn next(&self, byte: u8) -> Self {
        let vector = self.vector.derivative(byte).normalize();
        let unicode = self.unicode.as_ref().map(|x| x.next(byte));
        let mut result = Self {
            vector,
            last_success: self.last_success,
            unicode,
        };
        result.last_success = result.accepting_state().or(result.last_success);
        result
    }
    pub fn accepting_state(&self) -> Option<usize> {
        if self
            .unicode
            .as_ref()
            .map(|x| !x.is_accept())
            .unwrap_or(false)
        {
            return None;
        }
        self.vector.accepting_state()
    }
    pub fn is_rejecting_state(&self) -> bool {
        self.unicode
            .as_ref()
            .map(|x| x.is_reject())
            .unwrap_or(false)
            || self.vector.is_rejecting_state()
    }
    pub fn is_byte_sequence(&self) -> bool {
        match &self.unicode {
            None => self.vector.is_byte_sequence(),
            Some(state) => {
                let Some((_, seq)) = self.vector.as_byte_sequence() else {
                    return false;
                };
                state.accept_sequence(&seq)
            }
        }
    }
    pub fn as_byte_sequence(&self) -> Option<(usize, Vec<u8>)> {
        match &self.unicode {
            None => self.vector.as_byte_sequence(),
            Some(state) => {
                let (idx, seq) = self.vector.as_byte_sequence()?;
                if state.accept_sequence(&seq) {
                    Some((idx, seq))
                } else {
                    None
                }
            }
        }
    }
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

    if state.is_byte_sequence() {
        return;
    }

    let classes = state.approximate_congruence_class();
    let mut transitions = Vec::with_capacity(classes.len());

    for intervals in classes {
        let char = intervals.representative();
        let target = state.next(char);
        if !target.is_rejecting_state() {
            transitions.push((intervals, target.clone()));
            if !dfa.contains_key(&target) {
                explore_dfa_node(dfa, target, state_id)
            }
        }
    }

    dfa.get_mut(&state).unwrap().transitions = transitions;
}

pub fn build_dfa(state: DfaState) -> DfaTable {
    let mut state_id = 0;
    let mut dfa = HashMap::new();
    explore_dfa_node(&mut dfa, state, &mut state_id);
    #[cfg(pag_print_dfa)]
    print_dfa(&dfa);
    dfa
}

fn extract_leaf_states(dfa: &mut DfaTable, initial_state: &DfaState) -> HashSet<DfaState> {
    // TODO: switch to `drain_filter` (nightly) / `extract_if` (hashbrown)
    let leaf_states = dfa
        .iter()
        .filter_map(|(state, info)| {
            if info.transitions.is_empty() && state != initial_state {
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
