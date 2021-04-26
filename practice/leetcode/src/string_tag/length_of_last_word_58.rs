/*
 * @lc app=leetcode.cn id=58 lang=rust
 *
 * [58] 最后一个单词的长度
 */

// @lc code=start
impl Solution {
    pub fn length_of_last_word(s: String) -> i32 {
        let s = s.trim_end().chars().collect::<Vec<_>>();
        for i in (0..s.len()).rev() {
            if s[i]==' ' {
                return (s.len() - 1 - i) as i32
            }
        }
        s.len() as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_length_of_last_word() {
        assert_eq!(Solution::length_of_last_word("Hello World".to_string()), 5);
        assert_eq!(Solution::length_of_last_word("Hello World ".to_string()), 5);
        assert_eq!(Solution::length_of_last_word("".to_string()), 0);
    }
}