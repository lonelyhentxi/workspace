/*
 * @lc app=leetcode.cn id=560 lang=rust
 *
 * [560] 和为K的子数组
 */

// @lc code=start
use std::collections::HashMap;

impl Solution {
    pub fn subarray_sum(nums: Vec<i32>, k: i32) -> i32 {
        let mut count = 0;
        let mut util_here = HashMap::<i32,i32>::new();
        let mut offset = 0; // current axis zero
        for n in nums {
            util_here.entry(offset).and_modify(|v| *v+=1).or_insert(1);
            offset -= n;
            util_here.entry(k+offset).and_modify(|v| count += *v);
        }
        count
    }
}
// @lc code=end

struct Solution;


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_subarray_sum() {
        assert_eq!(Solution::subarray_sum(vec![1,1,1], 2),2);
    }
}
