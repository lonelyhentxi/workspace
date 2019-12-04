/*
 * @lc app=leetcode.cn id=64 lang=rust
 *
 * [64] 最小路径和
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn min_path_sum(grid: Vec<Vec<i32>>) -> i32 {
        let n = grid.len();
        if n==0 {
            return 0;
        }
        let m = grid[0].len();
        if m==0 {
            return 0;
        }
        let mut dp = vec![vec![0;m];n];
        dp[0][0] = grid[0][0];
        for i in 0..n {
            for j in 0..m {
                if i==0 && j==0 {
                    continue;
                }
                let left = if j==0 {
                    i32::max_value()
                } else {
                    dp[i][j-1]
                };
                let top = if i==0 {
                    i32::max_value()
                } else {
                    dp[i-1][j]
                };
                dp[i][j] = cmp::min(top,left) + grid[i][j];
            }
        }
        dp[n-1][m-1]
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_expected() {
        assert_eq!(Solution::min_path_sum(vec![
            vec![1,3,1],
            vec![1,5,1],
            vec![4,2,1]
        ]), 7);
    }
}

