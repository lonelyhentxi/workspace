/*
 * @lc app=leetcode.cn id=1 lang=rust
 *
 * [1] 两数之和
 */
// @lc code=start
use std::collections::HashMap;
impl Solution {
    pub fn two_sum(nums: Vec<i32>, target: i32) -> Vec<i32> {
        let mut mem = HashMap::with_capacity(nums.len());
        for (i,n) in nums.iter().enumerate() {
            mem.insert(n, i);
        }
        for (i,n) in nums.iter().enumerate() {
            let goal = target - n;
            if let Some(goal_index) = mem.get(&goal) {
                if *goal_index!=i {
                    return vec![i as i32,*goal_index as i32];
                }
            }
        }
        panic!("imposible!")
    }
}
// @lc code=end


struct Solution;

#[cfg(test)]
mod tests {
    #[test]
    fn returns_expected() {
        let nums = vec![2,7,11,15];
        let target = 9;
        assert_eq!(super::Solution::two_sum(nums, target),vec![0,1]);
    }
}