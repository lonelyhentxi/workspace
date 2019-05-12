use crate::constant::{
    CharLiteralPattern, DecPattern, DefsPattern, HexPattern, OctPattern, PdefsPattern,
    RdefsPattern, StringLiteralPattern,
};
use crate::core::LexPattern;
use crate::identifier::IdentifierPattern;
use crate::keyword::KeywordPattern;
use crate::misc::SpacePattern;
use crate::operator::OperatorPattern;
use std::sync::Arc;

pub fn gen_patterns() -> Vec<Arc<dyn LexPattern>> {
    let mut res: Vec<Arc<dyn LexPattern>> = vec![];
    res.push(Arc::new(KeywordPattern::new()));
    res.push(Arc::new(IdentifierPattern::new()));
    res.push(Arc::new(HexPattern::new()));
    res.push(Arc::new(OctPattern::new()));
    res.push(Arc::new(DecPattern::new()));
    res.push(Arc::new(DefsPattern::new()));
    res.push(Arc::new(PdefsPattern::new()));
    res.push(Arc::new(RdefsPattern::new()));
    res.push(Arc::new(CharLiteralPattern::new()));
    res.push(Arc::new(StringLiteralPattern::new()));
    res.push(Arc::new(OperatorPattern::new()));
    res.push(Arc::new(SpacePattern::new()));
    res
}

#[cfg(test)]
mod tests {
    use constant::StringLiteralPattern;
    use core::LexPattern;
    use misc::gen_digit_reg;
    use regex_engine::core::reg_match;

    #[test]
    fn test_digit_set() {
        let digit_reg = gen_digit_reg();
        for i in 0..10 {
            assert_eq!(reg_match(digit_reg.clone(), &format!("{}", i)), true);
        }
    }

    #[test]
    fn test_string_literal() {
        let string_literal_pattern = StringLiteralPattern::new();
        assert_eq!(
            string_literal_pattern.is_match("\"hello, \\\"world!\""),
            true
        );
    }
}
