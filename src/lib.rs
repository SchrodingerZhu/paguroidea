

use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
mod type_system;

#[derive(Clone, Debug)]
struct UniqueSymbol(Rc<String>);

impl Hash for UniqueSymbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

impl PartialEq for UniqueSymbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for UniqueSymbol {}

pub trait Token: Eq + Hash + Debug + Clone {}

struct Term {}
