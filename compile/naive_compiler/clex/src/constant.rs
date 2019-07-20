use crate::core::{LexPattern, LexTable};
use crate::misc::{
    gen_digit_reg, gen_exp_reg, gen_fs_expr, gen_hex_const_expr, gen_hex_digit_reg, gen_is_expr,
    gen_letter_reg, gen_oct_const_expr,
};
use regex_engine::core::*;
use regex_engine::ext::*;
use regex_engine::reg;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::sync::Arc;

pub struct HexPattern {
    expr: Arc<RegexExpr>,
    min_len: usize,
    max_len: usize,
}

impl HexPattern {
    pub fn new() -> HexPattern {
        let hex_digit_expr: Arc<AltExpr> = gen_hex_digit_reg();
        let is_expr: Arc<RepeatExpr> = gen_is_expr();
        // 0[xX]{H}+{IS}?
        HexPattern {
            expr: gen_hex_const_expr(hex_digit_expr.clone(), is_expr.clone()),
            min_len: 1,
            max_len: 64,
        }
    }
}

impl LexPattern for HexPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("CONSTANT").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("CONSTANT", "CONSTANT");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}

pub struct OctPattern {
    expr: Arc<RegexExpr>,
    min_len: usize,
    max_len: usize,
}

impl OctPattern {
    pub fn new() -> OctPattern {
        let digit_expr: Arc<CharSetExpr> = gen_digit_reg();
        let is_expr: Arc<RepeatExpr> = gen_is_expr();
        // 0{D}+{IS}?
        OctPattern {
            expr: gen_oct_const_expr(digit_expr.clone(), is_expr.clone()),
            min_len: 1,
            max_len: 64,
        }
    }
}

impl LexPattern for OctPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("CONSTANT").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("CONSTANT", "CONSTANT");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}

pub struct DecPattern {
    expr: Arc<RegexExpr>,
    min_len: usize,
    max_len: usize,
}

impl DecPattern {
    pub fn new() -> DecPattern {
        let digit_expr: Arc<CharSetExpr> = gen_digit_reg();
        let is_expr: Arc<RepeatExpr> = gen_is_expr();
        // {D}+{IS}?
        DecPattern {
            expr: reg!(
                ConcatExpr,
                reg!(PlusExpr, digit_expr.clone()),
                reg!(OptionalExpr, is_expr.clone())
            ),
            min_len: 1,
            max_len: 64,
        }
    }
}

impl LexPattern for DecPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("CONSTANT").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("CONSTANT", "CONSTANT");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}

pub struct DefsPattern {
    expr: Arc<RegexExpr>,
    min_len: usize,
    max_len: usize,
}

impl DefsPattern {
    pub fn new() -> DefsPattern {
        let digit_expr: Arc<CharSetExpr> = gen_digit_reg();
        let exp_expr: Arc<SeqExpr> = gen_exp_reg(digit_expr.clone());
        let fs_expr: Arc<CharSetExpr> = gen_fs_expr();
        // {D}+{E}{FS}?
        DefsPattern {
            expr: reg!(
                SeqExpr,
                vec![
                    reg!(PlusExpr, digit_expr.clone()),
                    exp_expr.clone(),
                    reg!(OptionalExpr, fs_expr.clone())
                ]
            ),
            min_len: 1,
            max_len: 64,
        }
    }
}

impl LexPattern for DefsPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("CONSTANT").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("CONSTANT", "CONSTANT");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}

pub struct PdefsPattern {
    expr: Arc<RegexExpr>,
    min_len: usize,
    max_len: usize,
}

impl PdefsPattern {
    pub fn new() -> PdefsPattern {
        let digit_expr: Arc<CharSetExpr> = gen_digit_reg();
        let exp_expr: Arc<SeqExpr> = gen_exp_reg(digit_expr.clone());
        let fs_expr: Arc<CharSetExpr> = gen_fs_expr();
        // {D}+.{D}*({E})?{FS}?
        let expr: Arc<SeqExpr> = reg!(
            SeqExpr,
            vec![
                reg!(PlusExpr, digit_expr.clone()),
                reg!(MatchExpr, '.'),
                reg!(RepeatExpr, digit_expr.clone()),
                reg!(OptionalExpr, exp_expr.clone()),
                reg!(OptionalExpr, fs_expr.clone())
            ]
        );
        PdefsPattern {
            expr,
            min_len: 1,
            max_len: 64,
        }
    }
}

impl LexPattern for PdefsPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("CONSTANT").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("CONSTANT", "CONSTANT");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}

pub struct RdefsPattern {
    expr: Arc<RegexExpr>,
    min_len: usize,
    max_len: usize,
}

impl RdefsPattern {
    pub fn new() -> RdefsPattern {
        let digit_expr: Arc<CharSetExpr> = gen_digit_reg();
        let exp_expr: Arc<SeqExpr> = gen_exp_reg(digit_expr.clone());
        let fs_expr: Arc<CharSetExpr> = gen_fs_expr();
        // {D}*.{D}+({E})?{FS}?
        let expr: Arc<SeqExpr> = reg!(
            SeqExpr,
            vec![
                reg!(RepeatExpr, digit_expr.clone()),
                reg!(MatchExpr, '.'),
                reg!(PlusExpr, digit_expr.clone()),
                reg!(OptionalExpr, exp_expr.clone()),
                reg!(OptionalExpr, fs_expr.clone())
            ]
        );
        RdefsPattern {
            expr,
            min_len: 1,
            max_len: 64,
        }
    }
}

impl LexPattern for RdefsPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("CONSTANT").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("CONSTANT", "CONSTANT");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}

pub struct CharLiteralPattern {
    expr: Arc<RegexExpr>,
    min_len: usize,
    max_len: usize,
}

impl CharLiteralPattern {
    pub fn new() -> CharLiteralPattern {
        let letter_expr: Arc<CharSetExpr> = gen_letter_reg();
        // L?'(\\.|[^'])+'
        let expr: Arc<SeqExpr> = reg!(
            SeqExpr,
            vec![
                reg!(OptionalExpr, letter_expr.clone()),
                reg!(MatchExpr, '\''),
                reg!(
                    RepeatExpr,
                    reg!(
                        AltExpr,
                        reg!(ConcatExpr, reg!(StringMatchExpr, "\\"), reg!(AnyExpr)),
                        reg!(
                            NotInCharSetExpr,
                            HashSet::from_iter(['\'', '\\'].iter().cloned())
                        )
                    )
                ),
                reg!(MatchExpr, '\'')
            ]
        );
        CharLiteralPattern {
            expr,
            min_len: 3,
            max_len: 64,
        }
    }
}

impl LexPattern for CharLiteralPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("CONSTANT").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("CONSTANT", "CONSTANT");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}

pub struct StringLiteralPattern {
    expr: Arc<RegexExpr>,
    min_len: usize,
    max_len: usize,
}

impl StringLiteralPattern {
    pub fn new() -> StringLiteralPattern {
        let letter_expr: Arc<CharSetExpr> = gen_letter_reg();
        // L?"(\\.|[^"\\])*"
        let expr: Arc<SeqExpr> = reg!(
            SeqExpr,
            vec![
                reg!(OptionalExpr, letter_expr.clone()),
                reg!(MatchExpr, '"'),
                reg!(
                    RepeatExpr,
                    reg!(
                        AltExpr,
                        reg!(ConcatExpr, reg!(StringMatchExpr, "\\"), reg!(AnyExpr)),
                        reg!(
                            NotInCharSetExpr,
                            HashSet::from_iter(['"', '\\'].iter().cloned())
                        )
                    )
                ),
                reg!(MatchExpr, '"')
            ]
        );
        StringLiteralPattern {
            expr,
            min_len: 3,
            max_len: 511,
            // ascii string literal is 509, 509 + 1(") + 1(") + 1(end)
        }
    }
}

impl LexPattern for StringLiteralPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("STRING_LITERAL").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("STRING_LITERAL", "CONSTANT");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}
