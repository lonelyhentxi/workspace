use crate::core::{LexPattern, LexTable};
use lazy_static;
use regex_engine::core::reg_match;
use regex_engine::ext::StringSetExpr;
use regex_engine::reg;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::sync::Arc;

lazy_static! {
    static ref OPERATOR_PATTERN_EXPR: Arc<StringSetExpr> = {
        let operators = HashSet::from_iter(
            vec![
                "...", ">>=", "<<=", "+=", "-=", "*=", "/=", "%=", "&=", "^=", "|=", ">>", "<<",
                "++", "--", "->", "&&", "||", "<=", ">=", "==", "!=", ";", "{", "}", "<%", "%>",
                ",", ":", "=", ")", "(", "[", "]", "<:", ":>", ".", "!", "=", "&", "-", "~", "+",
                ".", "*", "/", "%", "<", ">", "^", "|", "?",
            ]
            .iter()
            .map(|x| x.to_string()),
        );
        let expr: Arc<StringSetExpr> = reg!(StringSetExpr, operators);
        expr
    };
    static ref OPERATOR_PATTERN_ALIAS: Arc<HashMap<String, String>> = {
        Arc::new(HashMap::from_iter(
            vec![("<:", "["), (":>", "]"), ("%>", "}"), ("<%", "{")]
                .iter()
                .map(|(x, y)| (x.to_string(), y.to_string())),
        ))
    };
    static ref OPERATOR_PATTERN_TYPE_MAPPING: Arc<HashMap<String, String>> = {
        let mut res = HashMap::new();
        res.insert("...".to_string(), "ELLIPSIS".to_string());
        res.insert(">>=".to_string(), "RIGHT_ASSIGN".to_string());
        res.insert("<<=".to_string(), "LEFT_ASSIGN".to_string());
        res.insert("+=".to_string(), "ADD_ASSIGN".to_string());
        res.insert("-=".to_string(), "SUB_ASSIGN".to_string());
        res.insert("*=".to_string(), "MUL_ASSIGN".to_string());
        res.insert("/=".to_string(), "DIV_ASSIGN".to_string());
        res.insert("%=".to_string(), "MOD_ASSIGN".to_string());
        res.insert("&=".to_string(), "AND_ASSIGN".to_string());
        res.insert("^=".to_string(), "XOR_ASSIGN".to_string());
        res.insert("|=".to_string(), "OR_ASSIGN".to_string());
        res.insert(">>".to_string(), "RIGHT_OP".to_string());
        res.insert("<<".to_string(), "LEFT_OP".to_string());
        res.insert("++".to_string(), "INC_OP".to_string());
        res.insert("--".to_string(), "DEC_OP".to_string());
        res.insert("->".to_string(), "PTR_OP".to_string());
        res.insert("&&".to_string(), "AND_OP".to_string());
        res.insert("||".to_string(), "OR_OP".to_string());
        res.insert("<=".to_string(), "LE_OP".to_string());
        res.insert(">=".to_string(), "GE_OP".to_string());
        res.insert("==".to_string(), "EQ_OP".to_string());
        res.insert("!=".to_string(), "NE_OP".to_string());
        Arc::new(res)
    };
}

pub struct OperatorPattern {
    expr: Arc<StringSetExpr>,
    min_len: usize,
    max_len: usize,
    alias: Arc<HashMap<String, String>>,
    type_mapping: Arc<HashMap<String, String>>,
}

impl OperatorPattern {
    pub fn new() -> OperatorPattern {
        let expr = OPERATOR_PATTERN_EXPR.clone();
        OperatorPattern {
            min_len: expr.min_len(),
            max_len: expr.max_len(),
            expr,
            alias: OPERATOR_PATTERN_ALIAS.clone(),
            type_mapping: OPERATOR_PATTERN_TYPE_MAPPING.clone(),
        }
    }
}

impl LexPattern for OperatorPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, target: &str) -> usize {
        let target = target.to_string();
        let alias_source = self.alias.get(&target).unwrap_or(&target);
        let typing = self.type_mapping.get(alias_source).unwrap_or(alias_source);
        table.get(typing).0
    }

    fn register(&self, table: &mut LexTable) {
        for kw in &self.expr.set {
            table.try_insert(&self.type_mapping.get(kw).unwrap_or(kw), "OPERATOR");
        }
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}
