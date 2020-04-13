/*
 * @lc app=leetcode.cn id=11 lang=rust
 *
 * [11] 盛最多水的容器
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn max_area(height: Vec<i32>) -> i32 {
        let mut i = 0;
        let mut j = height.len() - 1;
        let mut res = 0;
        while i < j {
            let h = cmp::min(height[i], height[j]);
            res = cmp::max(h * (j-i) as i32, res);
            while i < j && h == height[i] { i+=1 };
            while i < j && h == height[j] { j-=1 };
        }
        res
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    #[test]
    fn returns_expected() {
        assert_eq!(super::Solution::max_area(vec![1,8,6,2,5,4,8,3,7]),49);
    }
}