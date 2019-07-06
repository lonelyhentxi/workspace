use crate::core::{LexPattern, LexTable};
use regex_engine::core::reg_match;
use regex_engine::ext::StringSetExpr;
use regex_engine::reg;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::sync::Arc;

lazy_static! {
    static ref KEYWORD_PATTERN_EXPR: Arc<StringSetExpr> = {
        reg!(
            StringSetExpr,
            HashSet::from_iter(
                vec![
                    "auto", "break", "case", "char", "const", "continue", "default", "do",
                    "double", "else", "enum", "extern", "float", "for", "goto", "if", "int",
                    "long", "register", "return", "short", "signed", "sizeof", "static", "struct",
                    "switch", "typedef", "union", "unsigned", "void", "volatile", "while"
                ]
                .into_iter()
                .map(|x| x.to_string())
            )
        )
    };
}

pub struct KeywordPattern {
    expr: Arc<StringSetExpr>,
    min_len: usize,
    max_len: usize,
}

impl KeywordPattern {
    pub fn new() -> KeywordPattern {
        let expr = KEYWORD_PATTERN_EXPR.clone();
        KeywordPattern {
            min_len: expr.min_len(),
            max_len: expr.max_len(),
            expr,
        }
    }
}

impl LexPattern for KeywordPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, target: &str) -> usize {
        table.get(&target.to_ascii_uppercase()).0
    }

    fn register(&self, table: &mut LexTable) {
        for kw in &self.expr.set {
            table.try_insert(&kw.to_ascii_uppercase(), "KEYWORD");
        }
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}
