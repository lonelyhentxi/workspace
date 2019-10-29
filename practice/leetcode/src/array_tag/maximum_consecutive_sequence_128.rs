/*
 * @lc app=leetcode.cn id=128 lang=rust
 *
 * [128] 最长连续序列
 */

// @lc code=start
use std::collections::HashSet;
use std::cmp;

impl Solution {
    pub fn longest_consecutive(nums: Vec<i32>) -> i32 {
        let mut set = HashSet::new();
        for n in &nums {
            set.insert(*n);
        }
        let mut max_len = if nums.is_empty() {
            0usize
        } else {
            1usize
        };
        for n in &nums {
            if !set.contains(&(n-1)) {
                let mut current_count = 1usize;
                while set.contains(&(n + current_count as i32)) {
                    current_count+=1;
                }
                max_len = cmp::max(max_len, current_count);
            }
        }
        max_len as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_longest_consecutive() {
        assert_eq!(Solution::longest_consecutive(vec![100, 4, 200, 1, 3, 2]), 4);
    }
}

