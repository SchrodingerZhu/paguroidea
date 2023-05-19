use crate::congruence::{approximate_congruence_class, meet};
use crate::derivative::derivative;
use crate::intervals::Intervals;
use crate::normalization::normalize;
use crate::regex_tree::RegexTree;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[macro_export]
macro_rules! beautify_mangle {
    ($mangle:expr, $mangle_map:ident) => {
        if let Some(res) = $mangle_map.get(&$mangle) {
            format_ident!("{}", res)
        } else {
            unreachable!("state not in mangle map")
        }
    };
}

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
        I: Iterator<Item = Rc<RegexTree>>,
    {
        let regex_trees = iter.collect();
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
    pub fn derivative(&self, x: u32) -> Self {
        Vector {
            regex_trees: self
                .regex_trees
                .iter()
                .map(|t| Rc::new(derivative(t.clone(), x)))
                .collect(),
        }
    }
    pub fn accepting_state(&self) -> Option<usize> {
        self.regex_trees
            .iter()
            .enumerate()
            .filter_map(|t| if t.1.is_nullable() { Some(t.0) } else { None })
            .next()
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
    pub fn generate_dfa(&self, name: String) -> TokenStream {
        let normalized = self.normalize();
        let mut statdid = 0;
        let mut state_map = HashMap::new();
        let dfa = build_dfa(normalized.clone(), &mut statdid, &mut state_map);
        let name = format_ident!("{}", name);
        let states = dfa.keys().map(|x| beautify_mangle!(x, state_map));
        let initial = beautify_mangle!(normalized, state_map);
        let actions = dfa.iter().map(|(state, transitions)| {
            let state_enum = beautify_mangle!(state, state_map);
            if state.is_rejecting_state() {
                quote! {
                    States::#state_enum => return longest_match,
                }
            } else {
                let transitions = transitions.iter().map(|x| {
                    let condition = x.0.to_tokens();
                    let target = beautify_mangle!(&x.1, state_map);
                    quote!(#condition => States::#target)
                });
                match state.accepting_state() {
                    Some(x) => quote! {
                        States::#state_enum => {
                            longest_match.replace((#x, idx));
                            state = match c as u32 {
                                #(#transitions,)*
                                _ => unsafe { ::core::hint::unreachable_unchecked() }
                            }
                        },
                    },
                    None => quote! {
                        States::#state_enum => {
                            state = match c as u32 {
                                 #(#transitions,)*
                                _ => unsafe { ::core::hint::unreachable_unchecked() }
                            };
                        },
                    },
                }
            }
        });
        let accepting_actions = dfa.iter().filter_map(|(state, _)| {
            state.accepting_state().map(|rule| {
                let label = beautify_mangle!(state, state_map);
                quote! {
                    States::#label => {
                        longest_match.replace((#rule, input.len()));
                    }
                }
            })
        });
        quote! {
          fn #name(input: &str) -> Option<(usize, usize)> {
              enum States {
                    #(#states,)*
              };
              let mut state = States::#initial;
              let mut longest_match = None;
              for (idx, c) in input.chars().enumerate() {
                  match state {
                    #(#actions)*
                  };
              }
              match state {
                  #(#accepting_actions,)*
                  _ => ()
              }
              longest_match
          }
        }
    }
}

fn make_dfa_transition(
    dfa: &mut HashMap<Vector, Vec<(Intervals, Vector)>>,
    transition: Intervals,
    vector: Vector,
    stateid: &mut usize,
    state_map: &mut HashMap<Vector, String>,
) {
    let c = transition.representative();
    let derivative = vector.derivative(c).normalize();
    unsafe {
        dfa.get_mut(&vector)
            .unwrap_unchecked()
            .push((transition, derivative.clone()));
    }
    if !dfa.contains_key(&derivative) {
        dfa.insert(derivative.clone(), vec![]);
        state_map.insert(derivative.clone(), format!("S{}", *stateid));
        *stateid += 1;
        explore_dfa_node(dfa, derivative, stateid, state_map)
    }
}

fn explore_dfa_node(
    dfa: &mut HashMap<Vector, Vec<(Intervals, Vector)>>,
    state: Vector,
    stateid: &mut usize,
    state_map: &mut HashMap<Vector, String>,
) {
    let transitions = state.approximate_congruence_class();
    for transition in transitions {
        make_dfa_transition(dfa, transition, state.clone(), stateid, state_map);
    }
}

fn build_dfa(
    initial_state: Vector,
    stateid: &mut usize,
    state_map: &mut HashMap<Vector, String>,
) -> HashMap<Vector, Vec<(Intervals, Vector)>> {
    let mut dfa = HashMap::new();
    state_map.insert(initial_state.clone(), format!("S{}", *stateid));
    *stateid += 1;
    dfa.insert(initial_state.clone(), vec![]);
    explore_dfa_node(&mut dfa, initial_state, stateid, state_map);
    *stateid = 0;
    dfa
}
