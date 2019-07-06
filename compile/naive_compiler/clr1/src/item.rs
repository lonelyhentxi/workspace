use std::collections::BTreeSet;
use std::fmt;
use std::rc::Rc;

use super::{Grammar, Production, Symbol, FAKE};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Item {
    pub prod: Rc<Production>,
    pub look_ahead: Symbol,
    pub stack_top: usize,
}

impl Item {
    pub fn from_str<T>(from: T, to: Vec<T>, stacktop: usize, lookahead: T, g: &Grammar) -> Item
    where
        T: Into<String> + Clone,
    {
        let non_terminals: BTreeSet<String> = g
            .get_nonterminals()
            .iter()
            .map(|s| s.to_string())
            .cloned()
            .collect();

        let from: String = from.into();
        assert!(
            non_terminals.contains(&from),
            "From needs to be a Non Terminal! got {} and NonTerminals {:?}",
            from,
            non_terminals
        );

        let from = Symbol::Nonterminal(from);
        let to = to
            .into_iter()
            .map(|s| s.into())
            .map(|s| {
                if non_terminals.contains(&s) {
                    Symbol::Nonterminal(s)
                } else {
                    Symbol::Terminal(s)
                }
            })
            .collect();

        let lookahead: String = lookahead.into();
        let lookahead = if non_terminals.contains(&lookahead) {
            Symbol::Nonterminal(lookahead)
        } else {
            Symbol::Terminal(lookahead)
        };

        let prod = Production::new(from, to);
        Item::new(Rc::new(prod), stacktop, lookahead)
    }

    pub fn new(prod: Rc<Production>, stack_top: usize, look_ahead: Symbol) -> Item {
        Item {
            prod,
            stack_top,
            look_ahead,
        }
    }

    pub fn from_production(prod: Rc<Production>, lookahead: Symbol) -> Item {
        Item::new(prod, 0, lookahead)
    }

    pub fn set_to_string(items: &BTreeSet<Item>) -> String {
        items
            .iter()
            .map(|item| format!("{}", item))
            .collect::<Vec<String>>()
            .join(" ")
    }

    pub fn set_of_sets_to_string(set: &BTreeSet<Rc<BTreeSet<Item>>>) -> String {
        set.iter()
            .map(|cc_i| Item::set_to_string(cc_i))
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn is_complete(&self) -> bool {
        assert!(
            self.stack_top <= self.prod.tail.len(),
            "Stacktop out of bounds"
        );
        if self.stack_top == self.prod.tail.len() {
            true
        } else {
            false
        }
    }

    pub fn is_terminator(&self) -> bool {
        self.prod.head.as_str() == FAKE && self.stack_top == 1 && self.is_complete()
    }

    pub fn stacktop(&self) -> Option<&Symbol> {
        if self.stack_top == self.prod.tail.len() {
            // Item complete
            return None;
        } else if self.stack_top < self.prod.tail.len() {
            return self.prod.tail.get(self.stack_top);
        } else {
            panic!("Stacktop out of bounds")
        }
    }

    pub fn after_stacktop(&self) -> &[Symbol] {
        &self.prod.tail[self.stack_top + 1..]
    }

    pub fn after_stacktop_and_lookahead(&self) -> Vec<Symbol> {
        let head = self.after_stacktop();
        let tail = &[self.look_ahead.clone()];
        head.iter().chain(tail.iter()).cloned().collect()
    }

    pub fn clone_with_next_stacktop(&self) -> Item {
        let mut item = self.clone();
        if item.is_complete() {
            panic!(
                "Attempting to push item's stacktop when the item is already complete {:?}",
                self
            );
        }

        item.stack_top += 1;
        item
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let to: Vec<String> = self
            .prod
            .tail
            .iter()
            .map(|s| s.to_string())
            .cloned()
            .collect();
        let to_str: String = if self.stacktop() == None {
            format!("{} •", to.join(" "))
        } else {
            to.iter()
                .enumerate()
                .map(|(i, s)| {
                    if i == self.stack_top {
                        format!("• {}", s)
                    } else {
                        s.clone()
                    }
                })
                .collect::<Vec<String>>()
                .join(" ")
        };

        write!(f, "[{} -> {}, {}]", self.prod.head, to_str, self.look_ahead)
    }
}
