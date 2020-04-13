/*
 * @lc app=leetcode.cn id=62 lang=rust
 *
 * [62] 不同路径
 */

// @lc code=start
impl Solution {
    pub fn unique_paths(m: i32, n: i32) -> i32 {
        let m = m as usize;
        let n = n as usize;
        if m == 0 || n == 0 {
            return 1;
        }
        let mut dp = vec![vec![0;m];n];
        dp[0][0] = 1;
        for i in 0..n {
            for j in 0..m {
                if i==0 && j==0 {
                    continue;
                }
                let left = if j==0 {
                    0
                } else {
                    dp[i][j-1]
                };
                let top = if i==0 {
                    0
                } else {
                    dp[i-1][j]
                };
                dp[i][j] = left + top;
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
        assert_eq!(Solution::unique_paths(3,2), 3);
        assert_eq!(Solution::unique_paths(7,3), 28);
    }
}

