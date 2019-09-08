/*
 * @lc app=leetcode.cn id=20 lang=rust
 *
 * [20] 有效的括号
 */

// @lc code=start
use std::collections::HashMap;
use std::iter::FromIterator;

impl Solution {
    pub fn is_valid(s: String) -> bool {
        let pair = HashMap::<u8, u8>::from_iter(
            vec![(b')', b'('), (b']', b'['), (b'}', b'{')].into_iter(),
        );
        let mut stack: Vec<u8> = vec![];
        for c in s.as_bytes() {
            if pair.contains_key(c) {
                 match stack.pop() {
                     Some(v) => {
                         if pair[c]!=v {
                             return false;
                         }
                     },
                     None => { return false; }
                 }
            }
            else {
                stack.push(*c);
            }
        }
        stack.is_empty()
    }
}
// @lc code=end

struct Solution;


#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        let is_valid = |s: &str| Solution::is_valid(s.to_string());
        assert!(is_valid("()"));
        assert!(is_valid("()[]{}"));
        assert!(!is_valid("(]"));
        assert!(!is_valid("([)]"));
        assert!(is_valid("{[]}"));
    }
}