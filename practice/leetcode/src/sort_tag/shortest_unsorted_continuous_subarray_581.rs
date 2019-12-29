/*
 * @lc app=leetcode.cn id=581 lang=rust
 *
 * [581] 最短无序连续子数组
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn find_unsorted_subarray(nums: Vec<i32>) -> i32 {
        let mut front_max_dp = vec![0; nums.len()];
        let mut back_min_dp = vec![0; nums.len()];
        let mut front_max = i32::min_value();
        for i in 0..nums.len() {
            front_max_dp[i] = front_max;
            front_max = cmp::max(nums[i], front_max);
        }
        let mut back_min = i32::max_value();
        for i in (0..nums.len()).rev() {
            back_min_dp[i] = back_min;
            back_min = cmp::min(nums[i], back_min);
        }
        let mut head = -1;
        for i in 0..nums.len() {
            if front_max_dp[i] > nums[i] || back_min_dp[i] < nums[i] {
                head = i as i32;
                break;
            }
        }
        if head == -1 {
            return 0;
        }
        let mut tail = nums.len() as i32;
        for i in (0..nums.len()).rev() {
            if front_max_dp[i] > nums[i] || back_min_dp[i] < nums[i] {
                tail = i as i32;
                break;
            }
        }
        tail  - head + 1
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_unsorted_subarray() {
        assert_eq!(Solution::find_unsorted_subarray(vec![2, 6, 4, 8, 10, 9, 15]),5);
    }
}
