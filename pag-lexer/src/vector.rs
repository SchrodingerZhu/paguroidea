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

    pub fn mangle(&self) -> String {
        let mut s = "V".to_string();
        for regex_tree in &self.regex_trees {
            let mangled = regex_tree.mangle();
            s.push_str(&format!("{}{}", mangled.len(), mangled));
        }
        s
    }

    pub fn derivative(&self, x: u8) -> Self {
        Vector {
            regex_trees: self
                .regex_trees
                .iter()
                .map(|t| Rc::new(derivative(t.clone(), x)))
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

    pub fn generate_dfa2(
        &self,
        initial_idx: &TokenStream,
        optimizer: &mut LoopOptimizer,
        success_actions: &[TokenStream],
        failure_action: &TokenStream,
    ) -> TokenStream {
        let initial_state = {
            let initial_state = self.normalize();
            let last_success = initial_state.accepting_state();
            DfaState {
                state: initial_state,
                last_success,
            }
        };
        let mut dfa = build_dfa2(initial_state.state.clone());
        let leaf_states = extract_leaf_states2(&mut dfa);
        let initial_label = format_ident!("S{}", dfa.get(&initial_state).unwrap().state_id);
        let actions = dfa.iter().map(|(state, info)| {
            let label = format_ident!("S{}", info.state_id);

            if let Some((rule_idx, seq)) = state.state.as_byte_sequence() {
                let literal = Literal::byte_string(&seq);
                let length = seq.len();
                let on_success = &success_actions[rule_idx];
                return quote! {
                    State::#label => {
                        if input[idx..].starts_with(#literal) {
                            idx += #length;
                            #on_success
                        } else {
                            #failure_action
                        }
                    },
                };
            }
            let transitions = info.transitions.iter().map(|(interval, target)| {
                if leaf_states.contains(target) {
                    let rule_idx = target.state.accepting_state().unwrap();
                    let on_success = &success_actions[rule_idx];
                    return quote! { Some(#interval) => #on_success, };
                }
                let target_label = format_ident!("S{}", dfa.get(target).unwrap().state_id);
                quote! { Some(#interval) => state = State::#target_label, }
            });
            //
            let lookahead = optimizer.generate_lookahead2(&dfa, state);
            let otherwise = state
                .last_success
                .and_then(|x| success_actions.get(x))
                .unwrap_or(failure_action);
            quote! {
                State::#label => {
                    #lookahead
                    match input.get(idx) {
                        #(#transitions)*
                        _ => #otherwise,
                    }
                },
            }
        });

        let labels = dfa.values().map(|info| format_ident!("S{}", info.state_id));

        quote! {
            enum State {
                #(#labels,)*
            };
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

    pub fn generate_dfa(&self, name: String, optimizer: &mut LoopOptimizer) -> TokenStream {
        let initial_state = self.normalize();
        let mut dfa = build_dfa(initial_state.clone());
        let leaf_states = extract_leaf_states(&mut dfa);
        let initial_label = format_ident!("S{}", dfa.get(&initial_state).unwrap().0);
        let name = format_ident!("{name}");
        let actions = dfa.iter().map(|(state, (state_id, transitions))| {
            let label = format_ident!("S{state_id}");

            if let Some((rule_idx, seq)) = state.as_byte_sequence() {
                let literal = Literal::byte_string(&seq);
                let length = seq.len();
                return quote! {
                    State::#label => {
                        return if input[idx..].starts_with(#literal) {
                            (#rule_idx, idx + #length)
                        } else {
                            longest_match
                        };
                    },
                };
            }

            let accepting_state = state
                .accepting_state()
                .map(|rule_idx| quote! { longest_match = (#rule_idx, idx); });

            let transitions = transitions.iter().map(|(interval, target)| {
                if leaf_states.contains(target) {
                    let rule_idx = target.accepting_state().unwrap();
                    return quote! { #interval => return (#rule_idx, idx + 1), };
                }
                let target_label = format_ident!("S{}", dfa.get(target).unwrap().0);
                quote! { #interval => state = State::#target_label, }
            });

            let lookahead = optimizer.generate_lookahead(&dfa, state);
            let get_char = if lookahead.is_some() {
                Some(quote! { let Some(c) = input.get(idx) else { break }; })
            } else {
                None
            };
            quote! {
                State::#label => {
                    #lookahead
                    #accepting_state
                    #get_char
                    match c {
                        #(#transitions)*
                        _ => return longest_match,
                    }
                },
            }
        });

        let labels = dfa
            .iter()
            .map(|(_, (state_id, _))| format_ident!("S{state_id}"));

        let mut accepting_map = HashMap::<usize, Vec<usize>>::new(); // maybe use Vec<Vec<usize>>?
        for (state, (state_id, _)) in &dfa {
            let Some(rule_idx) = state.accepting_state() else { continue; };
            accepting_map
                .entry(rule_idx)
                .and_modify(|v| v.push(*state_id))
                .or_insert(vec![*state_id]);
        }
        let accepting_actions = accepting_map.into_iter().map(|(rule_idx, state_ids)| {
            let enums = state_ids.into_iter().map(|id| {
                let label = format_ident!("S{id}");
                quote! { State::#label }
            });
            quote! { #(#enums)|* => longest_match = (#rule_idx, idx), }
        });

        quote! {
            fn #name(input: &[u8]) -> (usize, usize) {
                enum State {
                    #(#labels,)*
                };
                let mut state = State::#initial_label;
                let mut longest_match = (usize::MAX, 0);
                let mut idx = 0;
                while idx < input.len() {
                    let c = unsafe { *input.get_unchecked(idx) };
                    match state {
                        #(#actions)*
                    };
                    idx += 1;
                }
                match state {
                    #(#accepting_actions)*
                    _ => {}
                }
                longest_match
            }
        }
    }
}

pub type DfaTable = HashMap<Vector, (usize /* ID */, Vec<(Intervals, Vector)>)>;

fn explore_dfa_node(dfa: &mut DfaTable, state: Vector, state_id: &mut usize) {
    dfa.insert(state.clone(), (*state_id, vec![]));
    *state_id += 1;

    if state.is_byte_sequence() {
        return;
    }

    let classes = state.approximate_congruence_class();
    let mut transitions = Vec::with_capacity(classes.len());

    for intervals in classes {
        let char = intervals.representative();
        let target = state.derivative(char).normalize();
        if !target.is_rejecting_state() {
            transitions.push((intervals, target.clone()));
            if !dfa.contains_key(&target) {
                explore_dfa_node(dfa, target, state_id)
            }
        }
    }

    dfa.get_mut(&state).unwrap().1 = transitions;
}

fn build_dfa(initial_state: Vector) -> DfaTable {
    let mut state_id = 0;
    let mut dfa = HashMap::new();
    explore_dfa_node(&mut dfa, initial_state, &mut state_id);
    dfa
}

fn extract_leaf_states(dfa: &mut DfaTable) -> HashSet<Vector> {
    // TODO: switch to `drain_filter` (nightly) / `extract_if` (hashbrown)
    let leaf_states = dfa
        .iter()
        .filter_map(|(state, (_, transitions))| {
            if transitions.is_empty() && state.accepting_state().is_some() {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DfaState {
    state: Vector,
    last_success: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct DfaInfo {
    state_id: usize,
    pub(crate) transitions: Vec<(Intervals, DfaState)>,
}

pub type DfaTable2 = HashMap<DfaState, DfaInfo>;

fn explore_dfa_node2(dfa: &mut DfaTable2, state: DfaState, state_id: &mut usize) {
    dfa.insert(
        state.clone(),
        DfaInfo {
            state_id: *state_id,
            transitions: vec![],
        },
    );
    *state_id += 1;

    if state.state.is_byte_sequence() {
        return;
    }

    let classes = state.state.approximate_congruence_class();
    let mut transitions = Vec::with_capacity(classes.len());

    for intervals in classes {
        let char = intervals.representative();
        let target = state.state.derivative(char).normalize();
        let last_success = target.accepting_state().or(state.last_success);
        let next = DfaState {
            state: target,
            last_success,
        };
        if !next.state.is_rejecting_state() {
            transitions.push((intervals, next.clone()));
            if !dfa.contains_key(&next) {
                explore_dfa_node2(dfa, next, state_id)
            }
        }
    }

    dfa.get_mut(&state).unwrap().transitions = transitions;
}

pub fn build_dfa2(state: Vector) -> DfaTable2 {
    let mut state_id = 0;
    let mut dfa = HashMap::new();
    let last_success = state.accepting_state();
    let state = DfaState {
        state,
        last_success,
    };
    explore_dfa_node2(&mut dfa, state, &mut state_id);
    dfa
}

fn extract_leaf_states2(dfa: &mut DfaTable2) -> HashSet<DfaState> {
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
