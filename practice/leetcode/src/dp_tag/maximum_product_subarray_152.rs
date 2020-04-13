/*
 * @lc app=leetcode.cn id=152 lang=rust
 *
 * [152] 乘积最大子序列
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn max_product(nums: Vec<i32>) -> i32 {
        let mut a = 1;
        let mut b = 1;
        let mut max_value = i32::min_value();
        for num in nums {
            let aa = num * a;
            let bb = num * b;
            a = cmp::min(num, cmp::min(aa,bb));
            b = cmp::max(num, cmp::max(aa,bb));
            max_value = cmp::max(max_value, b);
        }
        max_value
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_max_product() {
        assert_eq!(Solution::max_product(vec![2,3,-2,4]), 6);
        assert_eq!(Solution::max_product(vec![-2,0,-1]), 0);
    }
}

