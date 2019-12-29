/*
 * @lc app=leetcode.cn id=494 lang=rust
 *
 * [494] 目标和
 */

// @lc code=start
impl Solution {
    pub fn find_target_sum_ways(nums: Vec<i32>, s: i32) -> i32 {
        let max: usize = nums.iter().fold(0, |acc, num| acc+num.abs()) as usize;
        if s < -(max as i32) || s > max as i32 {
            return 0;
        }
        let size = (2 * max + 1) as usize;
        let mut dp = vec![vec![0;size];2];
        dp[0][max] = 1;
        let mut bound = 0usize;
        for (i,num) in nums.iter().enumerate() {
            let this_index = i & 1;
            let next_index = (i + 1) & 1;
            for j in max-bound..=max+bound {
                dp[next_index][j+*num as usize] += dp[this_index][j];
                dp[next_index][j-*num as usize] += dp[this_index][j];
                dp[this_index][j] = 0;
            }
            bound += num.abs() as usize;
        }
        dp[nums.len() & 1][max+s as usize] as i32
    }
}
// @lc code=end

struct Solution;


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_target_sum_ways() {
        assert_eq!(Solution::find_target_sum_ways(vec![1, 1, 1, 1, 1], 3),5);
        assert_eq!(Solution::find_target_sum_ways(vec![1],2),0);
    }
}

