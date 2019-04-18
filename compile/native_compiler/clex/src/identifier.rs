use std::rc::Rc;
use regex_engine::ext::*;
use regex_engine::core::*;
use crate::core::{LexPattern, LexTable};
use crate::misc::{gen_letter_reg, gen_digit_reg};
use regex_engine::reg;

pub struct IdentifierPattern {
    expr: Rc<ConcatExpr>,
    min_len: usize,
    max_len: usize,
}

impl IdentifierPattern {
    pub fn new() -> IdentifierPattern {
        let digit_expr: Rc<CharSetExpr> = gen_digit_reg();
        let letter_expr: Rc<CharSetExpr> = gen_letter_reg();
        // {L}({L}|{D})*
        let expr: Rc<ConcatExpr> = reg!(ConcatExpr,
                letter_expr.clone(),
                reg!(RepeatExpr, reg!(AltExpr,letter_expr.clone(),digit_expr.clone()))
            );
        IdentifierPattern {
            min_len: 1,
            max_len: 64,
            expr,
        }
    }
}

impl LexPattern for IdentifierPattern {
    fn get_boundary(&self) -> (usize, usize) {
        (self.min_len, self.max_len)
    }

    fn hook(&self, table: &LexTable, _target: &str) -> usize {
        table.get("identifier").0
    }

    fn register(&self, table: &mut LexTable) {
        table.try_insert("identifier","identifier");
    }

    fn is_match(&self, target: &str) -> bool {
        reg_match(self.expr.clone(), target)
    }
}