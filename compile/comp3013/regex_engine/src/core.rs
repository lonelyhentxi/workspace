use std::rc::Rc;

pub trait RegexExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool;

    fn reg_match(&self, target: &str) -> bool {
        self.match_apply(target, 0, Rc::new(|rest, i| {
            rest.len() == i
        }))
    }
    fn reg_search(&self, target: &str, i: usize) -> bool {
        self.match_apply(target, i, Rc::new(|_, _| {
            true
        })) || (target.len() > i && self.reg_search(target, i + 1))
    }
}

pub struct MatchExpr {
   pub sub: char
}

impl MatchExpr {
    pub fn new(sub: char) -> MatchExpr {
        MatchExpr { sub }
    }
}

impl RegexExpr for MatchExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let current_equal = match target.chars().nth(i) {
            Some(ch) => ch == self.sub,
            None => false
        };
        current_equal && check(target, i + 1)
    }
}

pub struct NotMatchExpr {
   pub sub: char
}

impl NotMatchExpr {
    pub fn new(sub: char) -> NotMatchExpr {
        NotMatchExpr { sub }
    }
}

impl RegexExpr for NotMatchExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let current_equal = match target.chars().nth(i) {
            Some(ch) => ch != self.sub,
            None => false
        };
        current_equal && check(target, i + 1)
    }
}

pub struct EpsilonExpr {}

impl EpsilonExpr {
    pub fn new() -> EpsilonExpr {
        EpsilonExpr {}
    }
}

impl RegexExpr for EpsilonExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        check(target, i)
    }
}

pub struct ConcatExpr {
   pub left: Rc<dyn RegexExpr>,
   pub right: Rc<dyn RegexExpr>,
}

impl ConcatExpr {
    pub fn new(left: Rc<dyn RegexExpr>, right: Rc<dyn RegexExpr>) -> ConcatExpr {
        ConcatExpr {
            left,
            right,
        }
    }
}

impl RegexExpr for ConcatExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        self.left.match_apply(target, i, Rc::new(|rest, ri|
            self.right.match_apply(rest, ri, check.clone())))
    }
}

pub struct AltExpr {
    pub left: Rc<dyn RegexExpr>,
    pub right: Rc<dyn RegexExpr>,
}

impl AltExpr {
    pub fn new(left: Rc<dyn RegexExpr>, right: Rc<dyn RegexExpr>) -> AltExpr {
        AltExpr {
            left,
            right,
        }
    }
}

impl RegexExpr for AltExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        self.left.match_apply(target, i, check.clone())
            || self.right.match_apply(target, i, check)
    }
}

pub struct RepeatExpr {
    pub sub: Rc<dyn RegexExpr>,
}

impl RepeatExpr {
    pub fn new(sub: Rc<dyn RegexExpr>) -> RepeatExpr {
        RepeatExpr { sub }
    }
}

impl RegexExpr for RepeatExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        self.sub.match_apply(target, i, Rc::new(|rest, ri| {
            self.match_apply(rest, ri, check.clone()) || check(rest, ri)
        })) || check(target, i)
    }
}

pub struct AnyExpr {}

impl AnyExpr {
    pub fn new() -> AnyExpr {
        AnyExpr{}
    }
}

impl RegexExpr for AnyExpr {
    fn match_apply<'a>(&self,
                       target: &str, i: usize,
                       check: Rc<'a + Fn(&str, usize) -> bool>) -> bool {
        let current_equal = match target.chars().nth(i) {
            Some(_) => true,
            None => false
        };
        current_equal && check(target, i + 1)
    }
}

#[macro_export]
macro_rules! reg {
    ( $y:ident $(,$x:expr)*) => {
        {
            use std::rc::Rc;
            let tmp_regex = Rc::new($y::new( $($x),*));
            tmp_regex
        }
    }
}

#[macro_export]
macro_rules! reg_static {
    ( $y:ident $(,$x:expr)*) => {
        {
            let tmp_regex_static = $y::new( $($x),*);
            tmp_regex_static
        }
    }
}


pub fn reg_match(reg: Rc<dyn RegexExpr>, target: &str) -> bool {
    reg.reg_match(target)
}

pub fn reg_match_static(reg: impl RegexExpr, target: &str) -> bool {
    reg.reg_match(target)
}

pub fn reg_search(reg: Rc<dyn RegexExpr>, target: &str, i: usize) -> bool {
    reg.reg_search(target, i)
}

pub fn reg_search_static(reg: impl RegexExpr, target: &str, i: usize) -> bool {
    reg.reg_search(target, i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_macro_and_reg_match() {
        assert_eq!(reg_match_static(reg_static!(MatchExpr,'a'), "a"), true);
        assert_eq!(reg_match(reg!(MatchExpr, 'a'), "a"), true);
    }

    #[test]
    fn test_match() {
        assert_eq!(reg_match(reg!(MatchExpr,'a'), "a"), true);
    }

    #[test]
    fn test_epsilon() {
        assert_eq!(reg_match(reg!(EpsilonExpr), ""), true);
    }

    #[test]
    fn test_concat() {
        assert_eq!(reg_match(reg!(ConcatExpr,
    reg ! (MatchExpr, 'a'),
    reg !(MatchExpr, 'b')), "ab"), true);
    }

    #[test]
    fn test_alt() {
        let alt_reg = reg!(AltExpr, reg !(MatchExpr, 'a'), reg ! (MatchExpr, 'b'));
        assert_eq!(reg_match(alt_reg.clone(), "a"), true);
        assert_eq!(reg_match(alt_reg.clone(), "b"), true);
    }

    #[test]
    fn test_repeat() {
        assert_eq!(reg_match(reg!(RepeatExpr,reg ! (MatchExpr, 'a')), "aaaaaa"), true);
    }

    #[test]
    fn test_not_match() {
        assert_eq!(reg_match(reg!(NotMatchExpr,'d'), "a"), true);
    }

    #[test]
    fn test_search() {
        assert_eq!(reg_search(reg!(MatchExpr,'a'), "aba", 1), true);
    }

    #[test]
    fn test_any() {
        assert_eq!(reg_match(reg!(AnyExpr), "a"), true);
        assert_eq!(reg_match(reg!(AnyExpr), "b"), true);
    }
}