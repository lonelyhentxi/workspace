/*
 * @lc app=leetcode.cn id=55 lang=rust
 *
 * [55] 跳跃游戏
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn can_jump(nums: Vec<i32>) -> bool {
        let mut reach = 0usize;
        for i in 0..nums.len() {
            if i>reach || reach >= nums.len() - 1 {
                break;
            }
            reach = cmp::max(reach, i + nums[i] as usize);
        }
        reach >= nums.len() - 1
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        assert!(Solution::can_jump(vec![2,3,1,1,4]));
    }
}