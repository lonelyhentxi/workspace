/*
 * @lc app=leetcode.cn id=42 lang=rust
 *
 * [42] 接雨水
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn trap(height: Vec<i32>) -> i32 {
        if height.is_empty() {
            return 0;
        }
        let mut left = 0usize;
        let mut right = (height.len() - 1) as usize;
        let mut res = 0i32;
        while left < right {
            let mn = cmp::min(height[left], height[right]);
            if height[left]==mn {
                left += 1;
                while left < right && height[left] < mn {
                    res += (mn - height[left]) as i32;
                    left += 1;
                }
            }
            else {
                right -=1;
                while left < right && height[right] < mn {
                    res += (mn - height[right]) as i32;
                    right -= 1;
                }
            }
        }
        res
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test] 
    fn returns_expected() {
        assert_eq!(Solution::trap(vec![0,1,0,2,1,0,1,3,2,1,2,1]),6);
    }
}

