/*
 * @lc app=leetcode.cn id=139 lang=rust
 *
 * [139] 单词拆分
 */

// @lc code=start
use std::collections::HashSet;
use std::iter::FromIterator;

impl Solution {
    pub fn word_break(s: String, word_dict: Vec<String>) -> bool {
        let s: Vec<char> = s.chars().collect();
        let set = HashSet::<String>::from_iter(word_dict.into_iter());
        let mut dp = vec![false;s.len() + 1];
        dp[0] = true;
        for i in 1..=s.len() {
            for j in 0..i {
                if dp[j] && set.contains(&s[j..i].iter().collect::<String>()) {
                    dp[i] = true;
                    break;
                }
            }
        }
        dp[s.len()]
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    use crate::utils::test_tools::map_to_string;

    #[test]
    fn tets_word_break() {
        assert!(Solution::word_break(
            "leetcode".to_string(),
            map_to_string(&["leet", "code"])
        ));
        assert!(Solution::word_break(
            "applepenapple".to_string(),
            map_to_string(&["apple", "pen"])
        ));
        assert!(!Solution::word_break(
            "catsandog".to_string(),
            map_to_string(&["cats", "dog", "sand", "and", "cat"])
        ));
    }
}
