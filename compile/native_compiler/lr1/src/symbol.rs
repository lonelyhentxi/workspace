use super::{EOF, FAKE, LAMBDA};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    Terminal(String),
    Nonterminal(String),
}

impl Symbol {
    pub fn new_terminal(s: &str) -> Symbol {
        Symbol::Terminal(s.to_string())
    }

    pub fn new_nonterminal(s: &str) -> Symbol {
        Symbol::Nonterminal(s.to_string())
    }

    pub fn fake() -> Symbol {
        Symbol::new_nonterminal(FAKE)
    }

    pub fn eof() -> Symbol {
        Symbol::new_terminal(EOF)
    }

    pub fn lambda() -> Symbol {
        Symbol::new_terminal(LAMBDA)
    }

    pub fn is_terminal(&self) -> bool {
        match self {
            Symbol::Terminal(_) => true,
            _ => false,
        }
    }

    pub fn is_non_terminal(&self) -> bool {
        !self.is_terminal()
    }

    pub fn to_string(&self) -> &String {
        match self {
            Symbol::Terminal(ref s) => s,
            Symbol::Nonterminal(ref s) => s,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Symbol::Terminal(ref s) => s,
            Symbol::Nonterminal(ref s) => s,
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
