use std::rc::Rc;
use std::collections::hash_set::HashSet;
use super::core::{RegexExpr, EpsilonExpr, ConcatExpr, RepeatExpr};
use std::usize;

pub struct OptionalExpr {
    pub sub: Rc<dyn RegexExpr>
}

impl OptionalExpr {
    pub fn new(sub: Rc<dyn RegexExpr>) -> OptionalExpr {
        OptionalExpr { sub }
    }
}

impl RegexExpr for OptionalExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        self.sub.match_apply(target, i, check.clone())
            || EpsilonExpr::new().match_apply(target, i, check)
    }
}

pub struct StringMatchExpr {
    pub sub: String
}

impl StringMatchExpr {
    pub fn new(sub: &str) -> StringMatchExpr {
        StringMatchExpr { sub: sub.to_owned() }
    }
}

impl RegexExpr for StringMatchExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let mut current_equal: bool = true;
        for j in 0 as usize..self.sub.len() {
            current_equal = match target.chars().nth(i + j) {
                Some(ch) => ch == self.sub.chars().nth(j).unwrap(),
                None => false
            };
            if current_equal == false { break; }
        }
        current_equal && check(target, i + self.sub.len())
    }
}

pub struct CharSetExpr {
    pub set: HashSet<char>
}

impl CharSetExpr {
    pub fn new(set: HashSet<char>) -> CharSetExpr {
        CharSetExpr { set }
    }
}

impl RegexExpr for CharSetExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let current_equal = match target.chars().nth(i) {
            Some(ref ch) => self.set.contains(ch),
            None => false
        };
        current_equal && check(target, i + 1)
    }
}

pub struct StringSetExpr {
    pub set: HashSet<String>
}

impl StringSetExpr {
    pub fn new(set: HashSet<String>) -> StringSetExpr {
        StringSetExpr { set }
    }

    pub fn max_len(&self) -> usize {
        self.set.iter().fold(0, |next, this| usize::max(next, this.len()))
    }

    pub fn min_len(&self) -> usize {
        self.set.iter().fold(usize::MAX, |next, this| usize::min(next, this.len()))
    }
}

impl RegexExpr for StringSetExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let mut current_equal = false;
        let mut j = self.min_len();
        let max_len = self.max_len();
        while j <= max_len {
            if i + j <= target.len() && self.set.contains(&target[i..i + j]) {
                current_equal = true;
                break;
            }
            j += 1;
        }
        if !current_equal { j = self.max_len() }
        current_equal && check(target, i + j)
    }
}

pub struct NotInCharSetExpr {
    pub set: HashSet<char>
}

impl NotInCharSetExpr {
    pub fn new(set: HashSet<char>) -> NotInCharSetExpr {
        NotInCharSetExpr { set }
    }
}

impl RegexExpr for NotInCharSetExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let current_equal = match target.chars().nth(i) {
            Some(ref ch) => !self.set.contains(ch),
            None => false
        };
        current_equal && check(target, i + 1)
    }
}

pub struct SeqExpr {
    pub seq: Vec<Rc<dyn RegexExpr>>
}

impl SeqExpr {
    pub fn new(seq: Vec<Rc<dyn RegexExpr>>) -> SeqExpr {
        SeqExpr { seq }
    }
}

impl RegexExpr for SeqExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let mut curr: Rc<RegexExpr> = reg!(EpsilonExpr);
        for i in 0..self.seq.len() {
            curr = reg!(ConcatExpr,curr, self.seq[i].clone());
        }
        curr.match_apply(target, i, check)
    }
}

pub struct PlusExpr {
    pub sub: Rc<dyn RegexExpr>
}

impl PlusExpr {
    pub fn new(sub: Rc<dyn RegexExpr>) -> PlusExpr {
        PlusExpr { sub }
    }
}

impl RegexExpr for PlusExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let concat_expr: Rc<ConcatExpr> = reg!(ConcatExpr,
            self.sub.clone(),
            reg!(RepeatExpr,self.sub.clone())
            );
        concat_expr.match_apply(target, i, check)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::super::core::{reg_match, MatchExpr};

    #[test]
    fn test_optional() {
        let optional_reg = reg!(OptionalExpr, reg!(MatchExpr, 'a'));
        assert_eq!(reg_match(optional_reg.clone(), "a"), true);
        assert_eq!(reg_match(optional_reg.clone(), ""), true);
    }

    #[test]
    fn test_string() {
        let string_reg = reg!(StringMatchExpr, "abcd");
        assert_eq!(reg_match(string_reg, "abcd"), true);
    }

    #[test]
    fn test_charset() {
        let mut set = HashSet::new();
        for i in 0..10 {
            set.insert((i + ('0' as i32)) as u8 as char);
        }
        let charset_reg = reg!(CharSetExpr, set );
        assert_eq!(reg_match(charset_reg, "1"), true);
    }

    #[test]
    fn test_seq() {
        let seq: Vec<Rc<RegexExpr>> = vec![reg!(MatchExpr,'1'), reg!(MatchExpr,'c'), reg!(MatchExpr,'2')];
        assert_eq!(reg_match(reg!(SeqExpr,seq), "1c2"), true);
    }

    #[test]
    fn test_plus() {
        assert_eq!(reg_match(reg!(PlusExpr,reg!(MatchExpr,'a')), "aaaa"), true);
    }

    #[test]
    fn test_not_in_charset() {
        let mut set = HashSet::new();
        for i in 0..8 {
            set.insert((i + ('0' as i32)) as u8 as char);
        }
        let charset_reg = reg!(NotInCharSetExpr, set );
        assert_eq!(reg_match(charset_reg, "9"), true);
    }

    #[test]
    fn test_string_set() {
        let keywords: HashSet<_> =
            vec!["auto", "break", "case",
                 "char", "const", "continue",
                 "default", "do", "double",
                 "else", "enum", "extern",
                 "float", "for", "goto",
                 "if", "int", "long",
                 "register", "return", "short",
                 "signed", "sizeof", "static",
                 "struct", "switch", "typedef",
                 "union", "unsigned", "void",
                 "volatile", "while"
            ].iter().map(|it| it.to_string()).collect();
        let keywords_expr: Rc<StringSetExpr> = reg!(StringSetExpr,keywords);
        assert_eq!(reg_match(keywords_expr.clone(), "switch"), true);
        assert_eq!(reg_match(keywords_expr.clone(), "switch1"), false);
    }
}
