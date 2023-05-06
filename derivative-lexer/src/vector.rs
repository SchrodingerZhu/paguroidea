use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::congruence::{approximate_congruence_class, meet};
use crate::derivative::derivative;
use crate::intervals::Intervals;
use crate::normalization::normalize;
use crate::regex_tree::RegexTree;

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
    pub fn new<I>(iter: I) -> Self where I : Iterator<Item=Rc<RegexTree>> {
        let regex_trees = iter.collect();
        Self { regex_trees }
    }
    pub fn derivative(&self, x: u32) -> Self {
        Vector {
            regex_trees: self.regex_trees.iter().map(|t| Rc::new(derivative(t.clone(), x))).collect()
        }
    }
    pub fn is_accepting_state(&self) -> bool {
        self.regex_trees.iter().any(|t| t.is_nullable())
    }
    pub fn is_rejecting_state(&self) -> bool {
        self.regex_trees.iter().all(|t| matches!(t.as_ref(), RegexTree::Bottom))
    }
    pub fn approximate_congruence_class(&self) -> Vec<Intervals> {
        // meet all congruence classes for each regex tree
       self.regex_trees.iter()
           .map(|x| approximate_congruence_class(x))
              .fold(None, |acc, x| match acc {
                None => Some(x),
                Some(acc) => Some(meet(acc.as_slice(), x.as_slice())),
              }).unwrap_or_else(|| vec![])
    }
    pub fn normalize(self) -> Self {
        let regex_trees = self.regex_trees.into_iter()
            .map(|x| normalize(x))
            .collect();
        Self { regex_trees }
    }
}

fn make_dfa_transition(dfa: &mut HashMap<Vector, Vec<(Intervals, Vector)>>, transition: Intervals, vector: Vector) {
    let c = transition.representative();
    let derivative = vector.derivative(c).normalize();
    unsafe {
        dfa.get_mut(&vector).unwrap_unchecked().push((transition, derivative.clone()));
    }
    if !dfa.contains_key(&derivative) {
        dfa.insert(derivative.clone(), vec![]);
        explore_dfa_node(dfa, derivative)
    }
}

fn explore_dfa_node(dfa: &mut HashMap<Vector, Vec<(Intervals, Vector)>>, state: Vector) {
    let transitions = state.approximate_congruence_class();
    for transition in transitions {
        make_dfa_transition(dfa, transition, state.clone());
    }
}

pub fn build_dfa(initial_state: Vector) -> HashMap<Vector, Vec<(Intervals, Vector)>> {
    let mut dfa = HashMap::new();
    dfa.insert(initial_state.clone(), vec![]);
    explore_dfa_node(&mut dfa, initial_state);
    dfa
}

pub fn print_dfa(dfa: &HashMap<Vector, Vec<(Intervals, Vector)>>) {
    for (state, transitions) in dfa {
        println!("State: {}", state);
        for (transition, next_state) in transitions {
            println!("Transition: {} -> {}", transition, next_state);
        }
        println!();
    }
}

