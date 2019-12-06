/*
 * @lc app=leetcode.cn id=3 lang=rust
 *
 * [3] 无重复字符的最长子串
 */

// @lc code=start
use std::collections::HashSet;

impl Solution {
    pub fn length_of_longest_substring(s: String) -> i32 {
        let mut dict = HashSet::<u8>::new();
        let mut max = 0 as usize;
        let mut start = 0 as usize;
        let mut end = 0 as usize;
        let bytes = s.as_bytes();
        while start < s.len() && end < s.len() {
            let c = &bytes[end];
            if dict.contains(c) {
                dict.remove(&bytes[start]);
                start += 1;
            } else {
                dict.insert(bytes[end]);
                end += 1;
                max = usize::max(max, end - start);
            }
        }
        max as i32
    }
}
// @lc code=end

struct Solution;

use std::collections::HashMap;

impl Solution {
    pub fn length_of_longest_substring_slow(s: String) -> i32 {
        let mut dict = HashMap::<char, usize>::new();
        let mut max = 0 as usize;
        let mut len = 0 as usize;
        for (i, c) in s.chars().enumerate() {
            let longer = dict.contains_key(&c);
            if longer {
                let mut last = 0;
                dict.entry(c).and_modify(|index| {
                    len = i - *index;
                    last = *index;
                    *index = i;
                });
                dict.retain(|_, v| *v > last);
            } else {
                dict.insert(c, i);
                len += 1;
            };
            max = usize::max(len, max);
        }
        max as i32
    }
}

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        assert_eq!(Solution::length_of_longest_substring("abcabcbb".into()), 3);
        assert_eq!(Solution::length_of_longest_substring("bbbbb".into()), 1);
        assert_eq!(Solution::length_of_longest_substring("pwwkew".into()), 3)
    }
}
