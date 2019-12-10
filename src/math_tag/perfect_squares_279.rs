/*
 * @lc app=leetcode.cn id=279 lang=rust
 *
 * [279] 完全平方数
 */

// @lc code=start
use std::i32;
use std::cmp;

impl Solution {
    pub fn num_squares_dp(n: i32) -> i32 {
        let mut n = n;
        while n % 4 == 0 { n /= 4; }
        if n % 8==7 { return 4; }
        let mut dp = vec![i32::max_value(); (n+1) as usize];
        dp[0] = 0;
        for i in 0..=n as usize {
            let mut j=1 as usize;
            while i+j*j<= (n as usize) {
                dp[i+j*j] = cmp::min(dp[i+j*j], dp[i] + 1);
                j+=1;
            }
        }
        dp[n as usize]
    }

    pub fn num_squares_math(n: i32) -> i32 {
        let mut n = n;
        while n % 4 == 0 { n /= 4; }
        if n % 8==7 { return 4; }
        let mut a = 0;
        while a*a <= n as i32 {
            let b = f64::from(n-a*a).sqrt() as i32;
            if a * a + b * b == n {
                return if a>0&&b>0 {2} else {1};
            }
            a+=1;
        }
        3
    }

    pub fn num_squares(n: i32) -> i32 {
        Solution::num_squares_math(n)
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_num_squares_math() {
        assert_eq!(Solution::num_squares_math(12),3);
        assert_eq!(Solution::num_squares_math(13),2);
    }

    #[test]
    fn test_num_squares_dp() {
        assert_eq!(Solution::num_squares_dp(12),3);
        assert_eq!(Solution::num_squares_dp(13),2);
    }
}