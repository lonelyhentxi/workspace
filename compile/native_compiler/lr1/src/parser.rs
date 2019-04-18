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
                        println!("NONE NONE NONE");
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
