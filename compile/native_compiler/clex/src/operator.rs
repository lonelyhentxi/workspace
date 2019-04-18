use std::rc::Rc;
use regex_engine::ext::StringSetExpr;
use crate::core::{LexPattern, LexTable};
use std::collections::{HashMap,HashSet};
use regex_engine::core::reg_match;
use regex_engine::reg;
use std::iter::FromIterator;

pub struct OperatorPattern {
    expr: Rc<StringSetExpr>,
    min_len: usize,
    max_len: usize,
    alias: HashMap<String, String>,
}

impl OperatorPattern {
   pub fn new() -> OperatorPattern {
        let operators = HashSet::from_iter(vec!["...", ">>=", "<<=",
                             "+=", "-=", "*=",
                             "/=", "%=", "&=",
                             "^=", "|=", ">>",
                             "<<", "++", "--",
                             "->", "&&", "||",
                             "<=", ">=", "==",
                             "!=", ";", "{",
                             "}", "<%", "%>",
                             ",", ":", "=",
                             ")", "(", "[", "]",
                             "<:", ":>", ".",
                             "!", "=", "&", "-", "~", "+", ".", "*", "/", "%", "<", ">", "^", "|", "?"
        ].iter().map(|x| x.to_string()));
        let expr: Rc<StringSetExpr> = reg!(StringSetExpr,operators);
        OperatorPattern {
            min_len: expr.min_len(),
            max_len: expr.max_len(),
            expr,
            alias: HashMap::from_iter(vec![("<:", "["), (":>", "]"), ("%>", "}"), ("<%", "{")]
                .iter().map(|(x,y)| (x.to_string(), y.to_string())))
        }
    }
}

impl LexPattern for OperatorPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, target: &str) -> usize {
        let mut alias_source = target;
        if self.alias.contains_key(target) {
            alias_source = &self.alias[target];
        }
        table.get(alias_source).0
    }

    fn register(&self, table: &mut LexTable) {
        for kw in &self.expr.set {
            table.try_insert(&kw, "operator");
        }
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}