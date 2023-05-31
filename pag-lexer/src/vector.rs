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
use proc_macro2::TokenStream;
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
            .fold(None, |acc, x| match acc {
                None => Some(x),
                Some(acc) => Some(meet(acc.as_slice(), x.as_slice())),
            })
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

    pub fn generate_dfa(&self, name: String, optimizer: &mut LoopOptimizer) -> TokenStream {
        let initial_state = self.normalize();
        let mut dfa = build_dfa(initial_state.clone());
        let leaf_states = extract_leaf_states(&mut dfa);
        let initial_label = format_ident!("S{}", dfa.get(&initial_state).unwrap().0);
        let name = format_ident!("{name}");
        let labels = dfa
            .iter()
            .map(|(_, (state_id, _))| format_ident!("S{state_id}"));
        let actions = dfa.iter().map(|(state, (state_id, transitions))| {
            let label = format_ident!("S{state_id}");

            let accepting_state = state
                .accepting_state()
                .map(|rule_idx| quote! { longest_match = (#rule_idx, idx); });

            let transitions = transitions.iter().filter_map(|(interval, target)| {
                if leaf_states.contains(target) {
                    let rule_idx = target.accepting_state().unwrap();
                    return Some(quote! { #interval => return (#rule_idx, idx + 1), });
                }
                let target_label = format_ident!("S{}", dfa.get(target).unwrap().0);
                Some(quote! { #interval => state = State::#target_label, })
            });

            match optimizer.generate_lookahead(&dfa, state) {
                Some(lookahead) => quote! {
                    State::#label => {
                        #lookahead
                        #accepting_state
                        if let Some(c) = input.get(idx) {
                            match c {
                                #(#transitions)*
                                _ => return longest_match,
                            }
                        } else {
                            break;
                        }
                    },
                },
                None => quote! {
                    State::#label => {
                        #accepting_state
                        match c {
                            #(#transitions)*
                            _ => return longest_match,
                        }
                    },
                },
            }
        });

        let accepting_actions = dfa.iter().filter_map(|(state, (state_id, _))| {
            state.accepting_state().map(|rule_idx| {
                let label = format_ident!("S{state_id}");
                quote! {
                    State::#label => longest_match = (#rule_idx, idx),
                }
            })
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

    let intervals = state.approximate_congruence_class();
    let mut transitions = Vec::with_capacity(intervals.len());

    for interval in intervals {
        let char = interval.representative();
        let target = state.derivative(char).normalize();
        if !target.is_rejecting_state() {
            transitions.push((interval, target.clone()));
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
            if transitions.len() == 0 && state.accepting_state().is_some() {
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
