/*
 * @lc app=leetcode.cn id=70 lang=rust
 *
 * [70] 爬楼梯
 */

// @lc code=start
impl Solution {
    pub fn climb_stairs(n: i32) -> i32 {
        let mut dp = vec![0,1,2];
        for i in 3..=n as usize {
            let sum = dp[i-1] + dp[i-2];
            dp.push(sum);
        }
        dp[n as usize]
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_expected() {
        assert_eq!(Solution::climb_stairs(2),2);
        assert_eq!(Solution::climb_stairs(3),3);
    }
}

