/*
 * @lc app=leetcode.cn id=10 lang=rust
 *
 * [10] 正则表达式匹配
 */

// @lc code=start
use std::rc::Rc;

trait RegexExpr {
    fn match_apply<'a>(
        &self, target: &[u8], i: usize, check: Rc<dyn 'a+Fn(&[u8],usize)->bool>,
    ) -> bool;

    fn reg_match(&self, target: &[u8]) -> bool {
        self.match_apply(target, 0, Rc::new(|rest, i| rest.len()==i))
    }
}

struct EpsilonExpr;

impl EpsilonExpr {
    pub fn new() -> EpsilonExpr {
        EpsilonExpr
    }
}

impl RegexExpr for EpsilonExpr {
    fn match_apply<'a>(&self, target: &[u8], i: usize, check: Rc<dyn 'a+Fn(&[u8],usize)->bool>) -> bool {
        check(target, i)
    }
}

struct RepeatExpr {
    pub sub: Rc<dyn RegexExpr>,
}

impl RepeatExpr {
    pub fn new(sub: Rc<dyn RegexExpr>) -> RepeatExpr {
        RepeatExpr { sub }
    }
}

impl RegexExpr for RepeatExpr {
    fn match_apply<'a>(&self, target: &[u8], i: usize, check: Rc<dyn 'a+Fn(&[u8],usize)->bool>) -> bool {
        check(target, i) || self.sub.match_apply(
            target, i, Rc::new(|rest, ri| self.match_apply(rest, ri, check.clone()) || check(rest, ri))
        )
    }
}

struct ConcatExpr {
    pub left: Rc<dyn RegexExpr>,
    pub right: Rc<dyn RegexExpr>,
}

impl ConcatExpr {
    pub fn new(left: Rc<dyn RegexExpr>, right: Rc<dyn RegexExpr>) -> ConcatExpr {
        ConcatExpr { left, right }
    }
}

impl RegexExpr for ConcatExpr {
    fn match_apply<'a>(&self, target: &[u8], i: usize, check: Rc<dyn 'a+Fn(&[u8],usize)->bool>) -> bool {
        self.left.match_apply(
            target, i, Rc::new(|rest, ri| self.right.match_apply(rest, ri, check.clone()))
        )
    }
}

struct AnyExpr {}

impl AnyExpr {
    pub fn new() -> AnyExpr {
        AnyExpr {}
    }
}

impl RegexExpr for AnyExpr {
    fn match_apply<'a>(&self, target: &[u8], i: usize, check: Rc<dyn 'a+Fn(&[u8],usize)->bool>) -> bool {
        i<target.len() && check(target, i+1)
    }
}

struct MatchExpr {
    pub sub: u8,
}

impl MatchExpr {
    pub fn new(sub: u8) -> MatchExpr {
        MatchExpr { sub }
    }
}

impl RegexExpr for MatchExpr {
    fn match_apply<'a>(&self, target: &[u8], i: usize, check: Rc<dyn 'a+Fn(&[u8],usize)->bool>) -> bool {
        i < target.len() && self.sub==target[i] && check(target, i+1)
    }
}

struct SolutionExpr {
    pub expr: Rc<dyn RegexExpr>
}

impl SolutionExpr {
    fn new(sub: String) -> SolutionExpr {
        let sub = sub.as_bytes();
        let mut regs: Vec<Rc<dyn RegexExpr>>  = vec![];
        for c in sub {
            if *c==b'.' {
                regs.push(Rc::new(AnyExpr::new()))
            } else if *c==b'*' {
                let last_reg = regs.pop().unwrap();
                regs.push(Rc::new(RepeatExpr::new(last_reg)))
            } else {
                regs.push(Rc::new(MatchExpr::new(*c)))
            }
        }
        SolutionExpr { expr:  SolutionExpr::concat(&regs, 0, regs.len()) }
    }

    fn concat(regs: &[Rc<dyn RegexExpr>], start: usize, end: usize) -> Rc<dyn RegexExpr> {
        if end - start == 0 {
            Rc::new(EpsilonExpr::new())
        }
        else if end - start == 1 {
            regs[start].clone()
        } else {
            Rc::new(ConcatExpr::new(regs[start].clone(), SolutionExpr::concat(regs, start+1, end)))
        }
    }
}

impl RegexExpr for SolutionExpr {
    fn match_apply<'b>(&self, target: &[u8], i: usize, check: Rc<dyn 'b+Fn(&[u8],usize)->bool>) -> bool {
       self.expr.match_apply(target, i, check)
    }
}

impl Solution {
    pub fn is_match(s: String, p: String) -> bool {
        let regex = SolutionExpr::new(p);
        regex.reg_match(s.as_bytes())
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod tests {
    use super::Solution;

    #[test]
    fn returns_expected() {
        assert!(!Solution::is_match("a".to_string(), "".to_string()));
        assert!(!Solution::is_match("aa".to_string(), "a".to_string()));
        assert!(Solution::is_match("aa".to_string(), "a*".to_string()));
        assert!(Solution::is_match("ab".to_string(), ".*".to_string()));
        assert!(Solution::is_match("aab".to_string(), "c*a*b".to_string()));
        assert!(!Solution::is_match("mississippi".to_string(), "mis*is*p*.".to_string()));
        assert!(!Solution::is_match("aaaaaaaaaaaaab".to_string(), "a*a*a*a*a*a*a*a*a*a*c".to_string()));
    }
}