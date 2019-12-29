/*
 * @lc app=leetcode.cn id=312 lang=rust
 *
 * [312] 戳气球
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn max_coins(nums: Vec<i32>) -> i32 {
        let n = nums.len();
        let mut ballons = vec![1];
        ballons.extend(nums);
        ballons.push(1);
        let mut dp = vec![vec![0;n+2];n+2];
        for len in 1..=n {
            for i in 1..=n - len + 1 {
                let j = i + len - 1;
                for k in i..=j {
                    let mut max = dp[i][j];
                    max = cmp::max(
                        max,
                        ballons[i-1] * ballons[k] * ballons[j+1] + dp[i][k-1] + dp[k+1][j]
                    );
                    dp[i][j] = max;
                }
            }
        }
        dp[1][n]
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_max_coins() {
        assert_eq!(Solution::max_coins(vec![3,1,5,8]),167);
    }
}