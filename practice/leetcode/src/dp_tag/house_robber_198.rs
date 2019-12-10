/*
 * @lc app=leetcode.cn id=198 lang=rust
 *
 * [198] 打家劫舍
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn rob(nums: Vec<i32>) -> i32 {
        let n = nums.len();
        let mut dp = vec![0i32;nums.len()];
        let mut res = 0;
        for i in 0..n {
            dp[i] = (
                if (i as isize)-2>=0 {
                    res
                } else {
                    0
                }
            ) + nums[i];
            res = if i>0 {cmp::max(res,dp[i-1])} else { res };
        }
        if n==0 {
            0
        } else if n==1 {
            dp[n-1]
        } else {
            cmp::max(dp[n-1],dp[n-2])
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_robber() {
        assert_eq!(Solution::rob(vec![1,2,3,1]), 4);
        assert_eq!(Solution::rob(vec![2,7,9,3,1]),12);
    }
}