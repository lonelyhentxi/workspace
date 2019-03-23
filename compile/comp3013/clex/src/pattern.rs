use std::rc::Rc;
use crate::core::LexPattern;
use crate::keyword::KeywordPattern;
use crate::constant::{HexPattern,OctPattern,DecPattern,
               DefsPattern,PdefsPattern,RdefsPattern,
               CharLiteralPattern,StringLiteralPattern};
use crate::identifier::{IdentifierPattern};
use crate::operator::OperatorPattern;
use crate::misc::SpacePattern;


pub fn gen_patterns() -> Vec<Rc<dyn LexPattern>> {
    let mut res: Vec<Rc<dyn LexPattern>> = vec!();
    res.push(Rc::new(KeywordPattern::new()));
    res.push(Rc::new(IdentifierPattern::new()));
    res.push(Rc::new( HexPattern::new()));
    res.push(Rc::new( OctPattern::new()));
    res.push(Rc::new( DecPattern::new()));
    res.push(Rc::new( DefsPattern::new()));
    res.push(Rc::new( PdefsPattern::new()));
    res.push(Rc::new( RdefsPattern::new()));
    res.push(Rc::new( CharLiteralPattern::new()));
    res.push(Rc::new( StringLiteralPattern::new()));
    res.push(Rc::new( OperatorPattern::new()));
    res.push(Rc::new( SpacePattern::new()));
    res
}

#[cfg(test)]
mod tests {
    use misc::gen_digit_reg;
    use regex_engine::core::reg_match;
    use constant::StringLiteralPattern;
    use core::LexPattern;

    #[test]
    fn test_digit_set() {
        let digit_reg = gen_digit_reg();
        for i in 0..10 {
            assert_eq!(reg_match(digit_reg.clone(), &format!("{}", i)), true);
        }
    }

    #[test]
    fn test_string_literal() {
        let string_literal_pattern =  StringLiteralPattern::new();
        assert_eq!(string_literal_pattern.is_match("\"hello, \\\"world!\""),true);
    }
}