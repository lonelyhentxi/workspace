/*
 * @lc app=leetcode.cn id=72 lang=rust
 *
 * [72] 编辑距离
 */

// @lc code=start
use std::cmp::min;

#[allow(clippy::needless_range_loop)]
impl Solution {
    pub fn min_distance(word1: String, word2: String) -> i32 {
        let word1 = word1.as_bytes();
        let word2 = word2.as_bytes();
        let m = word1.len();
        let n = word2.len();
        let mut dp = vec![vec![0;n+1];m+1];
        for i in 0..=m {
            dp[i][0] = i;
        }
        for i in 0..=n {
            dp[0][i] = i;
        }
        for i in 1..=m {
            for j in 1..=n {
                if word1[i-1] == word2[j-1] {
                    dp[i][j] = dp[i-1][j-1];
                } else {
                    dp[i][j] = min(dp[i-1][j-1], min(dp[i-1][j], dp[i][j-1])) + 1;
                }
            }
        }
        dp[m][n] as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_expected() {
        assert_eq!(Solution::min_distance("horse".to_string(), "ros".to_string()),3);
        assert_eq!(Solution::min_distance("intention".to_string(), "execution".to_string()), 5);
    }
}

