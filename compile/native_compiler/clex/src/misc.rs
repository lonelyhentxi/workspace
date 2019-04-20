use crate::core::{LexPattern, LexTable};
use lazy_static;
use regex_engine::core::*;
use regex_engine::ext::*;
use regex_engine::reg;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::sync::Arc;

lazy_static! {
    static ref DIGIT_REG: Arc<CharSetExpr> = {
        let digit_set: HashSet<char> = HashSet::from_iter(
            ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
                .iter()
                .cloned(),
        );
        reg!(CharSetExpr, digit_set)
    };
}

pub fn gen_digit_reg() -> Arc<CharSetExpr> {
    DIGIT_REG.clone()
}

lazy_static! {
    static ref LETTER_SET: Arc<CharSetExpr> = {
        let mut letter_set = HashSet::new();
        for i in ('a' as i32)..=('z' as i32) {
            letter_set.insert(i as u8 as char);
        }
        for i in ('A' as i32)..=('Z' as i32) {
            letter_set.insert(i as u8 as char);
        }
        letter_set.insert('_');
        reg!(CharSetExpr, letter_set)
    };
}

pub fn gen_letter_reg() -> Arc<CharSetExpr> {
    LETTER_SET.clone()
}

lazy_static! {
    static ref HEX_DIGIT_REG: Arc<AltExpr> = {
        let mut letter_set = HashSet::new();
        for i in ('a' as i32)..=('f' as i32) {
            letter_set.insert(i as u8 as char);
        }
        for i in ('A' as i32)..=('F' as i32) {
            letter_set.insert(i as u8 as char);
        }
        reg!(AltExpr, reg!(CharSetExpr, letter_set), DIGIT_REG.clone())
    };
}

pub fn gen_hex_digit_reg() -> Arc<AltExpr> {
    HEX_DIGIT_REG.clone()
}

pub fn gen_exp_reg(digit_reg: Arc<RegexExpr>) -> Arc<SeqExpr> {
    reg!(
        SeqExpr,
        vec![
            reg!(AltExpr, reg!(MatchExpr, 'E'), reg!(MatchExpr, 'e')),
            reg!(
                OptionalExpr,
                reg!(AltExpr, reg!(MatchExpr, '+'), reg!(MatchExpr, '-'))
            ),
            reg!(PlusExpr, digit_reg)
        ]
    )
}

lazy_static! {
    static ref FS_EXPR: Arc<CharSetExpr> = {
        let fs_set: HashSet<char> = HashSet::from_iter(['f', 'F', 'l', 'L'].iter().cloned());
        reg!(CharSetExpr, fs_set)
    };
}

pub fn gen_fs_expr() -> Arc<CharSetExpr> {
    FS_EXPR.clone()
}

lazy_static! {
    static ref IS_EXPR: Arc<RepeatExpr> = {
        let is_sub_set: HashSet<char> = HashSet::from_iter(['u', 'U', 'l', 'L'].iter().cloned());
        reg!(RepeatExpr, reg!(CharSetExpr, is_sub_set))
    };
}

pub fn gen_is_expr() -> Arc<RepeatExpr> {
    IS_EXPR.clone()
}

pub fn gen_hex_const_expr(hex_digit_expr: Arc<RegexExpr>, is_expr: Arc<RegexExpr>) -> Arc<SeqExpr> {
    reg!(
        SeqExpr,
        vec![
            reg!(MatchExpr, '0'),
            reg!(AltExpr, reg!(MatchExpr, 'x'), reg!(MatchExpr, 'X')),
            reg!(PlusExpr, hex_digit_expr),
            reg!(OptionalExpr, is_expr)
        ]
    )
}

pub fn gen_oct_const_expr(digit_expr: Arc<RegexExpr>, is_expr: Arc<RegexExpr>) -> Arc<SeqExpr> {
    reg!(
        SeqExpr,
        vec![
            reg!(MatchExpr, '0'),
            reg!(PlusExpr, digit_expr),
            reg!(OptionalExpr, is_expr)
        ]
    )
}

lazy_static! {
    static ref SPACE_PATTERN_EXPR: Arc<CharSetExpr> = {
        reg!(
            CharSetExpr,
            HashSet::from_iter(['\t', '\n', '\r', ' '].iter().cloned())
        )
    };
}

pub struct SpacePattern {
    expr: Arc<CharSetExpr>,
    min_len: usize,
    max_len: usize,
}

impl SpacePattern {
    pub fn new() -> SpacePattern {
        SpacePattern {
            min_len: 1,
            max_len: 1,
            expr: SPACE_PATTERN_EXPR.clone(),
        }
    }
}

impl LexPattern for SpacePattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, target: &str) -> usize {
        table.get(target).0
    }

    fn register(&self, table: &mut LexTable) {
        for kw in &self.expr.set {
            table.try_insert(&kw.to_string(), "SPACE");
        }
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}
