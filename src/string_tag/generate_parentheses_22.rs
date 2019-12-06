/*
 * @lc app=leetcode.cn id=22 lang=rust
 *
 * [22] 括号生成
 */

// @lc code=start
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

impl Solution {
    pub fn generate_parenthesis(n: i32) -> Vec<String> {
        let mut mem = HashMap::<i32, Rc<HashSet<String>>>::new();
        Solution::generate_parenthesis_with_memorize(n, &mut mem).iter().cloned().collect::<Vec<_>>()
    }

    fn generate_parenthesis_with_memorize(n: i32, mem: & mut HashMap<i32, Rc<HashSet<String>>>) -> Rc<HashSet<String>> {
        if mem.contains_key(&n) {
            return mem[&n].clone();
        }
        let mut res = HashSet::<String>::new();
        if n==1 {
            res.insert(String::from("()"));
        } else if n!=0 {
            for i in 1..=n/2 {
                let left =  Solution::generate_parenthesis_with_memorize(i, mem);
                let right = Solution::generate_parenthesis_with_memorize(n-i, mem);
                for l in left.iter() {
                    for r in right.iter() {
                        res.insert(l.clone() + &r);
                    }
                }
                for r in right.iter() {
                    for l in left.iter() {
                        res.insert(r.clone() + &l);
                    }
                }
            }
            for inner in Solution::generate_parenthesis_with_memorize(n-1, mem).iter() {
                res.insert(String::from("(")+ &inner + ")");
            }
        }
        mem.insert(n, Rc::new(res));
        mem[&n].clone()
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn returns_expected() {
        let res = vec!["((()))","(()())","(())()","()(())","()()()"].into_iter().map(String::from).collect::<HashSet<_>>();
        assert_eq!(Solution::generate_parenthesis(3).into_iter().collect::<HashSet<_>>(), res);
    }
}