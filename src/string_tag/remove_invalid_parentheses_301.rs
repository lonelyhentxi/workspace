/*
 * @lc app=leetcode.cn id=301 lang=rust
 *
 * [301] 删除无效的括号
 */

// @lc code=start
use std::collections::{HashSet,VecDeque};

impl Solution {
    fn is_valid(t: &[char]) -> bool {
        let mut count = 0;
        for c in t {
            if *c=='(' {
                count += 1;
            } else if *c==')' {
                count -= 1;
                if count < 0 {
                    return false;
                }
            }
        }
        count == 0
    }

    pub fn remove_invalid_parentheses(s: String) -> Vec<String> {
        let mut res = vec![];
        let mut visited = HashSet::<Vec<char>>::new();
        let mut queue = VecDeque::<Vec<char>>::new();
        queue.push_back(s.chars().collect());
        let mut found = false;
        while let Some(t) = queue.pop_front() {
            if Solution::is_valid(&t) {
                res.push(t);
                found = true;
            } else if !found {
                for i in 0..t.len() {
                    if t[i]!= '(' && t[i] != ')' {
                        continue;
                    }
                    let mut s = Vec::from(&t[0..i]);
                    s.extend(&t[i+1..t.len()]);
                    if !visited.contains(&s) {
                        queue.push_back(s.clone());
                        visited.insert(s);
                    }
                }
            }
        }
        res.into_iter()
            .map(|v| v.into_iter().collect::<String>())
            .collect()
    }
}
// @lc code=end

struct Solution;


#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        to_s,
        utils::test_tools::{
            map_to_string,
            assert_equivalent,
        }
    };

    #[test]
    fn test_remove_invalid_parentheses1() {
        assert_equivalent(
            &Solution::remove_invalid_parentheses(
                to_s!("()())()")
            ), 
            &map_to_string(&["()()()", "(())()"])
        );
    }

    #[test]
    fn test_remove_invalid_parentheses2() {
        assert_equivalent(
            &Solution::remove_invalid_parentheses(
                to_s!("(a)())()")
            ),
            &map_to_string(&["(a)()()", "(a())()"])
        );
    }

    #[test]
    fn test_remove_invalid_parentheses3() {
        assert_equivalent(
            &Solution::remove_invalid_parentheses(
                to_s!(")(")
            ), 
            &map_to_string(&[""])
        )
    }
}
