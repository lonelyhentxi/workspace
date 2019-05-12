use super::{Production, Symbol, FAKE};
use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;

#[derive(Debug)]
pub struct Grammar {
    pub target: Symbol,
    pub productions: Vec<Rc<Production>>,
    prod_map: HashMap<Symbol, Vec<Rc<Production>>>,
    first_map: HashMap<Symbol, BTreeSet<Symbol>>,
    symbols: BTreeSet<Symbol>,
}

impl Grammar {
    pub fn from_str<T>(target: T, nonterminals: Vec<T>, productions: Vec<(T, Vec<T>)>) -> Grammar
    where
        T: Into<String> + Clone,
    {
        let nonterminals: BTreeSet<String> =
            nonterminals.iter().cloned().map(|s| s.into()).collect();

        let productions = productions
            .into_iter()
            .map(|(head, tail)| {
                let head: String = head.into();
                assert!(
                    nonterminals.contains(&head),
                    "Unexpected terminal in prod.head"
                );
                let head = Symbol::Nonterminal(head);
                let tail = tail
                    .into_iter()
                    .map(|s| s.into())
                    .map(|s| {
                        if nonterminals.contains(&s) {
                            Symbol::Nonterminal(s)
                        } else {
                            Symbol::Terminal(s)
                        }
                    })
                    .collect();

                Rc::new(Production::new(head, tail))
            })
            .collect();

        let target: String = target.into();
        assert!(nonterminals.contains(&target), "Unexpected terminal target");
        let target = Symbol::Nonterminal(target);

        Grammar::new(target, productions)
    }

    pub fn new(target: Symbol, productions: Vec<Rc<Production>>) -> Grammar {
        assert!(target.is_non_terminal(), "Unexpected terminal target");

        let mut prod_map = HashMap::new();
        let mut symbols = {
            let mut set = BTreeSet::new();
            set.insert(target.clone());
            set
        };

        for prod in &productions {
            assert!(
                prod.head.is_non_terminal(),
                "Unexpected terminal in prod.from"
            );
            prod_map
                .entry(prod.head.clone())
                .or_insert(vec![])
                .push(prod.clone());
            symbols.insert(prod.head.clone());
            for s in &prod.tail {
                symbols.insert(s.clone());
            }
        }

        let mut grammar = Grammar {
            target,
            productions,
            prod_map,
            symbols,
            first_map: HashMap::new(),
        };

        grammar.first_map = grammar.calculate_firsts();
        grammar
    }

    pub fn get_productions(&self, head: &Symbol) -> Option<&Vec<Rc<Production>>> {
        if head.is_terminal() {
            None
        } else {
            self.prod_map.get(head)
        }
    }

    pub fn get_terminals(&self) -> BTreeSet<Symbol> {
        self.symbols
            .iter()
            .filter(|s| s.is_terminal())
            .cloned()
            .collect()
    }

    pub fn get_nonterminals(&self) -> BTreeSet<Symbol> {
        self.symbols
            .iter()
            .filter(|s| s.is_non_terminal())
            .cloned()
            .collect()
    }

    fn calculate_firsts(&self) -> HashMap<Symbol, BTreeSet<Symbol>> {
        let mut first_map: HashMap<Symbol, BTreeSet<Symbol>> = HashMap::new();
        let mut first_map_snapshot = HashMap::new();

        let lambda_set = vec![Symbol::lambda()].into_iter().collect();
        let specials = vec![Symbol::eof(), Symbol::lambda()].into_iter().collect();

        for t in self.get_terminals().union(&specials) {
            first_map.insert(t.clone(), vec![t.clone()].into_iter().collect());
        }

        for nt in &self.get_nonterminals() {
            first_map.insert(nt.clone(), vec![].into_iter().collect());
        }

        while first_map != first_map_snapshot {
            first_map_snapshot = first_map.clone();
            for prod in &self.productions {
                let rhs = prod
                    .tail
                    .iter()
                    .enumerate()
                    .take_while(|&(i, _)| {
                        i == 0
                            || first_map
                                .get(&prod.tail[i - 1])
                                .unwrap()
                                .contains(&Symbol::lambda())
                    })
                    .fold(BTreeSet::new(), |acc, (i, symbol)| {
                        let first_i = first_map.get(symbol).expect("Wrong symbol");
                        let next = if i == prod.tail.len() - 1 {
                            first_i.iter().cloned().collect()
                        } else {
                            first_i.difference(&lambda_set).cloned().collect()
                        };

                        acc.union(&next).cloned().collect()
                    });

                if let Some(first) = first_map.get_mut(&prod.head) {
                    *first = first.union(&rhs).cloned().collect();
                }
            }
        }

        first_map
    }

    pub fn first_of(&self, symbols: &Vec<Symbol>) -> Option<BTreeSet<Symbol>> {
        let lambda_set = vec![Symbol::lambda()].into_iter().collect();

        let first = symbols
            .iter()
            .enumerate()
            .take_while(|&(i, _)| {
                i == 0
                    || self
                        .first_map
                        .get(&symbols[i - 1])
                        .unwrap()
                        .contains(&Symbol::lambda())
            })
            .fold(BTreeSet::new(), |acc, (i, symbol)| {
                let first_i = self.first_map.get(symbol).expect("Wrong symbol");
                let next = if i == symbols.len() - 1 {
                    first_i.iter().cloned().collect()
                } else {
                    first_i.difference(&lambda_set).cloned().collect()
                };

                acc.union(&next).cloned().collect()
            });

        if first.is_empty() {
            None
        } else {
            Some(first)
        }
    }

    pub fn with_fake_goal(&self) -> Grammar {
        let fake_goal = Symbol::new_nonterminal(FAKE);
        let fake_prod = Production::new(fake_goal.clone(), vec![self.target.clone()]);
        let prods = [vec![Rc::new(fake_prod)], self.productions.clone()].concat();

        Grammar::new(fake_goal, prods)
    }
}

#[cfg(test)]
mod tests {
    use super::super::LAMBDA;
    use super::*;
    fn example_grammar() -> Grammar {
        let non_terminals = vec!["Goal", "Expr", "Expr'", "Term", "Term'", "Factor"];

        let prods = vec![
            ("Goal", vec!["Expr"]),
            ("Expr", vec!["Term", "Expr'"]),
            ("Expr'", vec!["+", "Term", "Expr'"]),
            ("Expr'", vec!["-", "Term", "Expr'"]),
            ("Expr'", vec![LAMBDA]),
            ("Term", vec!["Factor", "Term'"]),
            ("Term'", vec!["x", "Factor", "Term'"]),
            ("Term'", vec!["%", "Factor", "Term'"]),
            ("Term'", vec![LAMBDA]),
            ("Factor", vec!["(", "Expr", ")"]),
            ("Factor", vec!["num"]),
            ("Factor", vec!["name"]),
        ];

        Grammar::from_str("Goal", non_terminals, prods)
    }

    #[test]
    fn grammar_creation() {
        use Symbol::*;
        let g = example_grammar();

        assert_eq!(
            g.get_terminals(),
            vec!["+", "-", "x", "%", LAMBDA, "(", ")", "num", "name"]
                .into_iter()
                .map(|s| s.to_string())
                .map(|s| Terminal(s))
                .collect()
        );

        assert_eq!(
            g.get_nonterminals(),
            vec!["Goal", "Expr", "Expr'", "Term", "Term'", "Factor"]
                .into_iter()
                .map(|s| s.to_string())
                .map(|s| Nonterminal(s))
                .collect()
        );
    }

    #[test]
    fn first_of_terminals() {
        let g = example_grammar();

        for t in &g.get_terminals() {
            assert_eq!(
                g.first_map.get(t).unwrap(),
                &vec![t.clone()].into_iter().collect()
            );
        }
    }

    #[test]
    fn first_of_non_terminals() {
        use Symbol::*;
        let g = example_grammar();

        let cases = vec![
            ("Goal", vec!["(", "name", "num"]),
            ("Expr", vec!["(", "name", "num"]),
            ("Expr'", vec!["+", "-", LAMBDA]),
            ("Term", vec!["(", "name", "num"]),
            ("Term'", vec!["x", "%", LAMBDA]),
            ("Factor", vec!["(", "name", "num"]),
        ];

        for &(ref nt, ref first) in &cases {
            let actual = g.first_map.get(&Nonterminal(nt.to_string())).unwrap();
            let expected = first
                .into_iter()
                .map(|s| s.to_string())
                .map(|s| Terminal(s))
                .collect::<BTreeSet<Symbol>>();

            assert_eq!(
                actual, &expected,
                "\nCase nt {:?}, first {:?}\nActual {:?}\nExpected {:?}",
                nt, first, actual, expected
            );
        }
    }

    #[test]
    fn first_of_symbols() {
        use Symbol::*;
        let g = example_grammar();
        assert_eq!(
            g.first_of(&vec![
                Nonterminal("Expr'".to_string()),
                Terminal("x".to_string())
            ])
            .unwrap(),
            vec!["+", "-", "x"]
                .into_iter()
                .map(|s| s.to_string())
                .map(|s| Terminal(s))
                .collect::<BTreeSet<Symbol>>()
        )
    }
}
