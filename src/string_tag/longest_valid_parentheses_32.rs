/*
 * @lc app=leetcode.cn id=32 lang=rust
 *
 * [32] 最长有效括号
 */

// @lc code=start
impl Solution {
    pub fn longest_valid_parentheses(s: String) -> i32 {
        let s = s.as_bytes();
        let mut dp = vec![0usize; s.len()];
        let mut max_len = 0usize;
        for i in 1..s.len() {
            let current = s[i];
            let last = s[i-1];
            if current == b')' {
                if last == b'(' {
                    dp[i] = if i>= 2 {
                        dp[i-2]
                    } else {
                        0
                    } + 2;
                } else if i - dp[i-1] > 0 && s[i - dp[i-1] - 1] == b'(' {
                    dp[i] = dp[i-1] + (if i-dp[i-1]>=2 {
                        dp[i-dp[i-1]-2]
                    } else {
                        0
                    }) + 2;
                }
                max_len = std::cmp::max(max_len,dp[i]);
            }
        }
        max_len as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        assert_eq!(Solution::longest_valid_parentheses("(()".to_string()),2);
        assert_eq!(Solution::longest_valid_parentheses(")()())".to_string()), 4);
    }
}

