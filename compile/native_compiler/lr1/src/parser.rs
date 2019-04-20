use super::{Grammar, IToken, Item, NodeId, Production, Symbol, Tree, EOF};
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Action {
    Accept,
    Reduce(Rc<Production>),
    Shift(Rc<BTreeSet<Item>>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum StackEl {
    Symbol((Symbol, Option<NodeId>)),
    State(Rc<BTreeSet<Item>>),
}

impl StackEl {
    pub fn is_symbol(&self) -> bool {
        match self {
            StackEl::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn unwrap_symbol(self) -> (Symbol, Option<NodeId>) {
        if let StackEl::Symbol(symbol) = self {
            return symbol;
        } else {
            panic!("Unexpected unwrap_symbol a non symbol StackEl")
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    grammar: Grammar,
    ss: BTreeSet<Rc<BTreeSet<Item>>>,
    goto_map: HashMap<(Rc<BTreeSet<Item>>, Symbol), BTreeSet<Rc<BTreeSet<Item>>>>,
    action: HashMap<(Rc<BTreeSet<Item>>, Symbol), BTreeSet<Action>>,

    index_to_ss: Vec<Rc<BTreeSet<Item>>>,
    ss_to_index: HashMap<Rc<BTreeSet<Item>>, usize>,

    stack: RefCell<Vec<StackEl>>,
}

impl Parser {
    pub fn new(g: Grammar) -> Parser {
        let mut p = Parser {
            grammar: g.with_fake_goal(),
            ss: BTreeSet::new(),
            goto_map: HashMap::new(),
            action: HashMap::new(),

            index_to_ss: Vec::new(),
            ss_to_index: HashMap::new(),

            stack: RefCell::new(Vec::new()),
        };

        p.build_ss();

        p
    }

    pub fn insert_ss(&mut self, ss_i: Rc<BTreeSet<Item>>) {
        assert!(!self.ss_to_index.contains_key(&ss_i));
        let index = self.index_to_ss.len();

        self.index_to_ss.push(ss_i.clone());
        self.ss_to_index.insert(ss_i.clone(), index);
    }

    pub fn closure(&self, items: &BTreeSet<Item>) -> Rc<BTreeSet<Item>> {
        let mut new_items = items.clone();
        let mut items = BTreeSet::new();

        while !new_items.is_subset(&items) {
            items = items.union(&new_items).cloned().collect();
            new_items.clear();

            let filtered_items = items
                .iter()
                .filter(|item| item.stacktop().is_some())
                .filter(|item| item.stacktop().unwrap().is_non_terminal())
                .filter(|item| {
                    self.grammar
                        .get_productions(item.stacktop().unwrap())
                        .is_some()
                });

            for item in filtered_items {
                for prod in self
                    .grammar
                    .get_productions(item.stacktop().unwrap())
                    .unwrap()
                {
                    let first = self.grammar.first_of(&item.after_stacktop_and_lookahead());
                    if first == None {
                        continue;
                    }

                    for b in first.unwrap() {
                        let item = Item::from_production(prod.clone(), b.clone());
                        new_items.insert(item);
                    }
                }
            }
        }

        Rc::new(items)
    }

    pub fn goto(&self, items: &BTreeSet<Item>, x: &Symbol) -> Option<Rc<BTreeSet<Item>>> {
        let next: BTreeSet<Item> = items
            .iter()
            .filter(|&item| item.stacktop().is_some())
            .filter(|&item| item.stacktop().unwrap() == x)
            .map(|item| item.clone_with_next_stacktop())
            .collect();

        if next.is_empty() {
            return None;
        } else {
            //println!("GOTO>>>next {:?}\n", Item::set_to_string(&next));
            Some(self.closure(&next))
        }
    }

    fn build_ss(&mut self) {
        let ss0 = {
            let item = Item::from_production(self.grammar.productions[0].clone(), Symbol::eof());
            let mut set = BTreeSet::new();
            set.insert(item);
            self.closure(&set)
        };

        let mut ss = BTreeSet::new();
        let mut new_ss = {
            let mut set = BTreeSet::new();
            set.insert(ss0.clone());
            set
        };

        self.insert_ss(ss0);

        let mut done = BTreeSet::new();

        while !new_ss.is_empty() {
            ss = ss.union(&new_ss).cloned().collect();
            new_ss.clear();

            for ss_i in &ss {
                if done.contains(ss_i) {
                    continue;
                }
                for item in ss_i.iter() {
                    if item.is_complete() {
                        let entry = self
                            .action
                            .entry((ss_i.clone(), item.look_ahead.clone()))
                            .or_insert(BTreeSet::new());

                        if item.is_terminator() {
                            entry.insert(Action::Accept);
                        } else {
                            entry.insert(Action::Reduce(item.prod.clone()));
                        }
                        continue;
                    }

                    let stacktop = item.stacktop().unwrap();
                    let next = self.goto(ss_i, &stacktop);
                    if next == None {
                        println!("NONE");
                        continue;
                    }
                    let next = next.unwrap();
                    if !ss.contains(&next) {
                        let is_new = new_ss.insert(next.clone());
                        if is_new {
                            self.insert_ss(next.clone());
                        }
                    }
                    if stacktop.is_terminal() {
                        let entry = self
                            .action
                            .entry((ss_i.clone(), stacktop.clone()))
                            .or_insert(BTreeSet::new());

                        entry.insert(Action::Shift(next.clone()));
                    } else {
                        let entry = self
                            .goto_map
                            .entry((ss_i.clone(), stacktop.clone()))
                            .or_insert(BTreeSet::new());
                        entry.insert(next.clone());
                    }
                }

                done.insert(ss_i.clone());
            }
        }
        self.ss = ss;
    }

    fn get_single_action(&self, key: &(Rc<BTreeSet<Item>>, Symbol)) -> Result<&Action, String> {
        let &(ref s, ref x) = key;
        let action = self.action.get(key);
        action
            .ok_or(format!(
                "Next action is empty.\nAction {}, {}, {} -> {:?}\nStack {}",
                self.ss_to_index.get(s).unwrap(),
                Item::set_to_string(s),
                x,
                action,
                self.stack_to_string()
            ))
            .and_then(|actions| {
                if actions.len() != 1 {
                    Err(format!("Found conflicts in the Action table"))
                } else {
                    Ok(actions)
                }
            })
            .map(|actions| actions.iter().take(1).collect::<Vec<&Action>>()[0])
    }

    fn get_single_goto(
        &self,
        key: &(Rc<BTreeSet<Item>>, Symbol),
    ) -> Result<&Rc<BTreeSet<Item>>, String> {
        self.goto_map
            .get(key)
            .ok_or(format!("Next state is empty"))
            .and_then(|states| {
                if states.len() != 1 {
                    Err(format!("Something really bad happened"))
                } else {
                    Ok(states)
                }
            })
            .map(|states| states.iter().take(1).collect::<Vec<&Rc<BTreeSet<Item>>>>()[0])
    }

    fn get_stack_top_state(&self) -> Result<Rc<BTreeSet<Item>>, String> {
        self.stack
            .borrow()
            .last()
            .ok_or(format!("Empty stack"))
            .and_then(|el| match el {
                &StackEl::State(ref s) => Ok(s.clone()),
                _ => Err(format!("Attempting to read an invalid state from stack")),
            })
    }

    pub fn is_lr1(&self) -> bool {
        self.action.iter().all(|(_, actions)| actions.len() <= 1)
    }

    pub fn parse<I>(&self, mut tokens: I) -> Result<Tree, String>
    where
        I: Iterator<Item = Box<IToken>>,
    {
        use Action::*;

        let mut tree = Tree::new();

        {
            let mut stack = self.stack.borrow_mut();
            *stack = vec![
                StackEl::Symbol((Symbol::eof(), None)),
                StackEl::State(self.index_to_ss.get(0).unwrap().clone()),
            ];
        }

        let mut word = {
            let word = tokens.next();
            if word.is_none() {
                return Ok(tree);
            }

            word.unwrap()
        };

        if word.kind() == EOF {
            return Ok(tree);
        }

        loop {
            let state = self.get_stack_top_state()?;
            let action =
                self.get_single_action(&(state.clone(), Symbol::new_terminal(word.kind())))?;

            match action {
                Reduce(ref prod) => {
                    let new_root = tree.new_node(prod.head.clone());
                    tree.set_root(new_root);

                    let to_pop = prod.tail.len() * 2;
                    let stack_len = self.stack.borrow().len();

                    if to_pop > stack_len {
                        return Err(format!("Reduce Error: empty stack"));
                    }

                    let popped = self.stack.borrow_mut().split_off(stack_len - to_pop);
                    let _propped = popped
                        .into_iter()
                        .filter(|el| el.is_symbol())
                        .map(|el| el.unwrap_symbol())
                        .map(|(_, child_id)| child_id)
                        .map(|child_id| tree.append(new_root, child_id.expect("Unexpected EOF")))
                        .collect::<Vec<()>>();

                    let state = self.get_stack_top_state()?;
                    let next = self.get_single_goto(&(state, prod.head.clone()))?;
                    self.stack
                        .borrow_mut()
                        .push(StackEl::Symbol((prod.head.clone(), Some(new_root))));
                    self.stack.borrow_mut().push(StackEl::State(next.clone()));
                }

                Shift(ref next_state) => {
                    let mut stack = self.stack.borrow_mut();
                    let new_symbol = Symbol::new_terminal(word.kind());
                    let node_id = tree.new_node(word);
                    stack.push(StackEl::Symbol((new_symbol, Some(node_id))));
                    stack.push(StackEl::State(next_state.clone()));

                    word = tokens
                        .next()
                        .ok_or(format!("Unexpected end of token stream"))?;
                }

                &Accept => {
                    return Ok(tree);
                }
            }
        }
    }

    pub fn stack_to_string(&self) -> String {
        self.stack
            .borrow()
            .iter()
            .map(|el| match el {
                &StackEl::Symbol((ref s, ref node_id)) => format!("{} {:?}", s, node_id),
                &StackEl::State(ref s) => self.ss_to_index.get(s).unwrap().to_string(),
            })
            .collect::<Vec<String>>()
            .join(", ")
    }

    pub fn print_ss(&self) {
        println!("SS");
        println!("======");

        for (i, ss_i) in self.index_to_ss.iter().enumerate() {
            println!("{:<4} {}", i, Item::set_to_string(ss_i));
        }
        println!("\n");
    }

    pub fn print_tables(&self) {
        println!();
        println!("ACTION");
        println!("======");

        for (&(ref ss_i, ref symbol), action) in &self.action {
            let i = self.ss_to_index.get(ss_i).unwrap();
            let a = self.set_of_actions_to_string(&action);
            println!("{:<4} {:<4} -> {}", i, symbol, a);
        }

        println!();
        println!("GOTO");
        println!("======");

        for (&(ref ss_i, ref symbol), next) in &self.goto_map {
            let i = self.ss_to_index.get(ss_i).unwrap();
            let j = next
                .iter()
                .map(|next| self.ss_to_index.get(next).unwrap())
                .map(|j| j.to_string())
                .collect::<Vec<String>>()
                .join(", ");
            println!("{:<4} {:<4} -> {}", i, symbol, j);
        }
    }

    pub fn action_to_string(&self, action: &Action) -> String {
        match action {
            &Action::Accept => "Accept".to_string(),
            &Action::Reduce(ref prod) => format!("{}", prod),
            &Action::Shift(ref ss_i) => format!(
                "Shift({})",
                self.ss_to_index
                    .get(ss_i)
                    .expect("action_to_string: bad shift")
            ),
        }
    }

    pub fn set_of_actions_to_string(&self, action: &BTreeSet<Action>) -> String {
        action
            .iter()
            .map(|action| self.action_to_string(&action))
            .collect::<Vec<String>>()
            .join(", ")
    }

    pub fn pretty_print_tables(&self) {
        println!();
        println!("ACTION");
        println!("======");
        let mut rows: Vec<Vec<String>> = vec![];

        let mut first_row = vec!["".to_string(), EOF.to_string()];
        first_row.append(
            &mut self
                .grammar
                .get_terminals()
                .iter()
                .map(|s| s.to_string())
                .cloned()
                .collect(),
        );

        rows.push(first_row);

        let mut terminals = vec![EOF.to_string()];
        terminals.append(
            &mut self
                .grammar
                .get_terminals()
                .iter()
                .map(|s| s.to_string())
                .cloned()
                .collect(),
        );

        for (i, ss_i) in self.index_to_ss.iter().enumerate() {
            let mut row = vec![i.to_string()];
            for t in &terminals {
                let action = self.action.get(&(ss_i.clone(), Symbol::new_terminal(t)));
                if action == None {
                    row.push("".to_string());
                } else {
                    row.push(self.set_of_actions_to_string(action.unwrap()));
                }
            }

            rows.push(row);
        }

        for row in rows {
            for (i, cell) in row.iter().enumerate() {
                if i == 0 {
                    print!("{:<4}", cell);
                } else {
                    print!("{:<30}", cell);
                }
            }
            println!();
        }

        println!();
        println!("GOTO");
        println!("====");
        let mut rows: Vec<Vec<String>> = vec![];

        let mut first_row = vec!["".to_string()];
        first_row.append(
            &mut self
                .grammar
                .get_nonterminals()
                .iter()
                .map(|s| s.to_string())
                .cloned()
                .collect(),
        );

        rows.push(first_row);

        for (i, ss_i) in self.ss.iter().enumerate() {
            let mut row = vec![i.to_string()];
            for nt in &self.grammar.get_nonterminals() {
                let next = self.goto_map.get(&(ss_i.clone(), nt.clone()));
                if next == None {
                    row.push("".to_string());
                } else {
                    row.push(self.ss_to_index.get(ss_i).unwrap().to_string());
                }
            }
            rows.push(row);
        }

        for row in rows {
            for (i, cell) in row.iter().enumerate() {
                if i == 0 {
                    print!("{:<4}", cell);
                } else {
                    print!("{:<10}", cell);
                }
            }
            println!();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::{EOF, FAKE};
    use super::*;

    #[test]
    fn closure_and_goto_test() {
        let parser = example_parser();
        let g = &parser.grammar;
        let first_prod = parser.grammar.productions[0].clone();
        let item = Item::from_production(first_prod, Symbol::Terminal(EOF.to_string()));
        let items: BTreeSet<Item> = vec![item].iter().cloned().collect();
        let cc0 = parser.closure(&items);

        let actual = &cc0;
        let expected = vec![
            Item::from_str(FAKE, vec!["List"], 0, EOF, g),
            Item::from_str("List", vec!["List", "Pair"], 0, EOF, g),
            Item::from_str("List", vec!["List", "Pair"], 0, "(", g),
            Item::from_str("List", vec!["Pair"], 0, EOF, g),
            Item::from_str("List", vec!["Pair"], 0, "(", g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, EOF, g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, "(", g),
            Item::from_str("Pair", vec!["(", ")"], 0, EOF, g),
            Item::from_str("Pair", vec!["(", ")"], 0, "(", g),
        ]
        .iter()
        .cloned()
        .collect();

        let expected = Rc::new(expected);

        assert_eq!(
            actual,
            &expected,
            "\n\n>>>actual {}\n>>>expected {}",
            Item::set_to_string(&actual),
            Item::set_to_string(&expected)
        );

        let actual = parser
            .goto(&cc0, &Symbol::Terminal("(".to_string()))
            .unwrap();
        let expected = vec![
            Item::from_str("Pair", vec!["(", "Pair", ")"], 1, EOF, g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 1, "(", g),
            Item::from_str("Pair", vec!["(", ")"], 1, EOF, g),
            Item::from_str("Pair", vec!["(", ")"], 1, "(", g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, ")", g),
            Item::from_str("Pair", vec!["(", ")"], 0, ")", g),
        ]
        .iter()
        .cloned()
        .collect();
        let expected = Rc::new(expected);

        assert_eq!(
            actual,
            expected,
            "\n\n>>>actual {}\n>>>expected {}",
            Item::set_to_string(&actual),
            Item::set_to_string(&expected)
        );
    }

    #[test]
    fn goto_test2() {
        use Symbol::*;
        let parser = example_parser();
        let cc_vec = paretheses_ss();

        let col = vec![
            Nonterminal("Goal".to_string()),
            Nonterminal("List".to_string()),
            Nonterminal("Pair".to_string()),
            Terminal("(".to_string()),
            Terminal(")".to_string()),
            Symbol::eof(),
        ];

        let expected = vec![
            [
                None,
                Some(cc_vec[1].clone()),
                Some(cc_vec[2].clone()),
                Some(cc_vec[3].clone()),
                None,
                None,
            ],
            [
                None,
                None,
                Some(cc_vec[4].clone()),
                Some(cc_vec[3].clone()),
                None,
                None,
            ],
            [None, None, None, None, None, None],
            [
                None,
                None,
                Some(cc_vec[5].clone()),
                Some(cc_vec[6].clone()),
                Some(cc_vec[7].clone()),
                None,
            ],
            [None, None, None, None, None, None],
            [None, None, None, None, Some(cc_vec[8].clone()), None],
            [
                None,
                None,
                Some(cc_vec[9].clone()),
                Some(cc_vec[6].clone()),
                Some(cc_vec[10].clone()),
                None,
            ],
            [None, None, None, None, None, None],
            [None, None, None, None, None, None],
            [None, None, None, None, Some(cc_vec[11].clone()), None],
            [None, None, None, None, None, None],
            [None, None, None, None, None, None],
        ];

        for (i, row) in expected.iter().enumerate() {
            for (j, e) in row.iter().enumerate() {
                let a = parser.goto(&cc_vec[i], &col[j]);

                assert_eq!(
                    a.clone(),
                    e.clone(),
                    "\nFrom {:?}\nActual {:?}\nExpected {:?}",
                    Item::set_to_string(&cc_vec[i]),
                    a.clone().map(|a| Item::set_to_string(&a)),
                    e.clone().map(|e| Item::set_to_string(&e)),
                );
            }
        }
    }

    #[test]
    fn action_test() {
        use Action::*;
        let parser = example_parser();
        let cc_vec = paretheses_ss();
        let prods = parser.grammar.productions.clone();

        let col = [EOF, "(", ")"];

        let expected = vec![
            //0
            [None, Some(Shift(cc_vec[3].clone())), None],
            [Some(Accept), Some(Shift(cc_vec[3].clone())), None],
            //2
            [
                Some(Reduce(prods[2].clone())),
                Some(Reduce(prods[2].clone())),
                None,
            ],
            [
                None,
                Some(Shift(cc_vec[6].clone())),
                Some(Shift(cc_vec[7].clone())),
            ],
            //4
            [
                Some(Reduce(prods[1].clone())),
                Some(Reduce(prods[1].clone())),
                None,
            ],
            [None, None, Some(Shift(cc_vec[8].clone()))],
            //6
            [
                None,
                Some(Shift(cc_vec[6].clone())),
                Some(Shift(cc_vec[10].clone())),
            ],
            [
                Some(Reduce(prods[4].clone())),
                Some(Reduce(prods[4].clone())),
                None,
            ],
            //8
            [
                Some(Reduce(prods[3].clone())),
                Some(Reduce(prods[3].clone())),
                None,
            ],
            [None, None, Some(Shift(cc_vec[11].clone()))],
            //10
            [None, None, Some(Reduce(prods[4].clone()))],
            [None, None, Some(Reduce(prods[3].clone()))],
        ];

        for (i, row) in expected.iter().enumerate() {
            for (j, e) in row.iter().enumerate() {
                let a = parser
                    .action
                    .get(&(cc_vec[i].clone(), Symbol::Terminal(col[j].to_string())));

                let e = e.clone().map(|a| {
                    let mut set = BTreeSet::new();
                    set.insert(a.clone());
                    set
                });

                assert_eq!(
                    a.map(|a| a.clone()),
                    e.clone(),
                    "\n>>>Actual {:?} \n>>>Expected {:?}",
                    a.map(|a| parser.set_of_actions_to_string(&a)),
                    e.map(|a| parser.set_of_actions_to_string(&a))
                );
            }
        }
    }

    #[test]
    fn build_ss_test() {
        let parser = example_parser();
        let expected_ss: BTreeSet<Rc<BTreeSet<Item>>> = paretheses_ss().into_iter().collect();
        let actual_ss = parser.ss.clone();

        assert_eq!(
            actual_ss.len(),
            expected_ss.len(),
            "Should have the same length \nACTUAL   {}\nEXPECTED {}",
            Item::set_of_sets_to_string(&actual_ss),
            Item::set_of_sets_to_string(&expected_ss)
        );

        for (actual_items, expected_items) in actual_ss.iter().zip(&expected_ss) {
            assert_eq!(
                actual_items,
                expected_items,
                "\n>>>Actual {}\n>>>Expected {}",
                Item::set_to_string(actual_items),
                Item::set_to_string(expected_items)
            );
        }

        assert!(parser.is_lr1());
    }

    #[test]
    fn tables_test() {
        let parser = example_parser();

        parser.print_ss();
        parser.print_tables();
        parser.pretty_print_tables();
    }

    #[test]
    fn parse_test() {
        fn lex(tokens: &str) -> Vec<Box<IToken>> {
            if tokens.len() == 0 {
                return vec![];
            }
            tokens
                .split(" ")
                .into_iter()
                .map(|s| Box::new((s.to_string(), "".to_string())) as Box<IToken>)
                .collect()
        }

        let parser = example_parser();

        let cases = vec![
            "",
            "EOF",
            "( ) EOF",
            "( ( ) ) EOF",
            "( ) ( ) EOF",
            "( ) ( ) ( ) EOF",
            "( ( ) ) ( ) EOF",
            "( ( ) ) ( ) ( ) EOF",
            "( ( ( ) ) ) ( ) EOF",
            "( ( ( ) ) ) ( ( ) ) EOF",
        ];

        for case in cases {
            let tokens = lex(case);
            let res = parser.parse(tokens.into_iter());
            assert!(res.is_ok(), "case {:?}, res {}", case, res.err().unwrap());

            println!("TREE of {}", case);
            res.unwrap().print();
        }
    }

    fn paretheses_grammar() -> Grammar {
        let non_terminals = vec!["List", "Pair"];

        let prods = vec![
            ("List", vec!["List", "Pair"]),
            ("List", vec!["Pair"]),
            ("Pair", vec!["(", "Pair", ")"]),
            ("Pair", vec!["(", ")"]),
        ];

        let g = Grammar::from_str("List", non_terminals, prods);
        g
    }

    fn example_parser() -> Parser {
        let g = paretheses_grammar();
        Parser::new(g)
    }

    fn paretheses_ss() -> Vec<Rc<BTreeSet<Item>>> {
        let g = paretheses_grammar().with_fake_goal();
        let cc0 = vec![
            Item::from_str(FAKE, vec!["List"], 0, EOF, &g),
            Item::from_str("List", vec!["List", "Pair"], 0, EOF, &g),
            Item::from_str("List", vec!["List", "Pair"], 0, "(", &g),
            Item::from_str("List", vec!["Pair"], 0, EOF, &g),
            Item::from_str("List", vec!["Pair"], 0, "(", &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, EOF, &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, "(", &g),
            Item::from_str("Pair", vec!["(", ")"], 0, EOF, &g),
            Item::from_str("Pair", vec!["(", ")"], 0, "(", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc1 = vec![
            Item::from_str(FAKE, vec!["List"], 1, EOF, &g),
            Item::from_str("List", vec!["List", "Pair"], 1, EOF, &g),
            Item::from_str("List", vec!["List", "Pair"], 1, "(", &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, EOF, &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, "(", &g),
            Item::from_str("Pair", vec!["(", ")"], 0, EOF, &g),
            Item::from_str("Pair", vec!["(", ")"], 0, "(", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc2 = vec![
            Item::from_str("List", vec!["Pair"], 1, EOF, &g),
            Item::from_str("List", vec!["Pair"], 1, "(", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc3 = vec![
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, ")", &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 1, EOF, &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 1, "(", &g),
            Item::from_str("Pair", vec!["(", ")"], 0, ")", &g),
            Item::from_str("Pair", vec!["(", ")"], 1, EOF, &g),
            Item::from_str("Pair", vec!["(", ")"], 1, "(", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc4 = vec![
            Item::from_str("List", vec!["List", "Pair"], 2, EOF, &g),
            Item::from_str("List", vec!["List", "Pair"], 2, "(", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc5 = vec![
            Item::from_str("Pair", vec!["(", "Pair", ")"], 2, EOF, &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 2, "(", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc6 = vec![
            Item::from_str("Pair", vec!["(", "Pair", ")"], 0, ")", &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 1, ")", &g),
            Item::from_str("Pair", vec!["(", ")"], 0, ")", &g),
            Item::from_str("Pair", vec!["(", ")"], 1, ")", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc7 = vec![
            Item::from_str("Pair", vec!["(", ")"], 2, EOF, &g),
            Item::from_str("Pair", vec!["(", ")"], 2, "(", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc8 = vec![
            Item::from_str("Pair", vec!["(", "Pair", ")"], 3, EOF, &g),
            Item::from_str("Pair", vec!["(", "Pair", ")"], 3, "(", &g),
        ]
        .iter()
        .cloned()
        .collect();

        let cc9 = vec![Item::from_str("Pair", vec!["(", "Pair", ")"], 2, ")", &g)]
            .iter()
            .cloned()
            .collect();

        let cc10 = vec![Item::from_str("Pair", vec!["(", ")"], 2, ")", &g)]
            .iter()
            .cloned()
            .collect();

        let cc11 = vec![Item::from_str("Pair", vec!["(", "Pair", ")"], 3, ")", &g)]
            .iter()
            .cloned()
            .collect();

        vec![cc0, cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9, cc10, cc11]
            .iter()
            .cloned()
            .map(|s| Rc::new(s))
            .collect()
    }
}
