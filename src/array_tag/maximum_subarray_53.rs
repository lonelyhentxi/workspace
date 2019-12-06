/*
 * @lc app=leetcode.cn id=53 lang=rust
 *
 * [53] 最大子序和
 */

// @lc code=start
use std::cmp;
impl Solution {
    pub fn max_sub_array(nums: Vec<i32>) -> i32 {
        let mut max = i32::min_value();
        let mut current = 0;
        for n in nums {
            current+=n;
            max = cmp::max(current, max);
            if current < 0 {
                current = 0;
            }
        }
        max
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        assert_eq!(Solution::max_sub_array(vec![-2,1,-3,4,-1,2,1,-5,4]),6);
        assert_eq!(Solution::max_sub_array(vec![-1]), -1);
    }
}

