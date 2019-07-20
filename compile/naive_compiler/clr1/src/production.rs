use std::fmt;

use super::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Production {
    pub head: Symbol,
    pub tail: Vec<Symbol>,
}

impl Production {
    pub fn new(head: Symbol, tail: Vec<Symbol>) -> Production {
        Production { head, tail }
    }
}

impl fmt::Display for Production {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.head,
            self.tail
                .iter()
                .map(|s| format!("{:?}", s))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}
