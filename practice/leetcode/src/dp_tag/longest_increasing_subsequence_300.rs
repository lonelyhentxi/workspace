/*
 * @lc app=leetcode.cn id=300 lang=rust
 *
 * [300] 最长上升子序列
 */

// @lc code=start
impl Solution {
    fn binary_search(nums: &[i32], target: i32) -> Option<usize> {
        if nums.is_empty() {
            None
        } else if target < nums[0] {
            Some(0usize)
        } else if target > nums[nums.len()-1] {
            None
        } else {
            let mut left = 0usize;
            let mut right = nums.len();
            while left < right {
                let mid = (left + right) / 2;
                if nums[mid] < target { left = mid + 1; }
                else { right = mid; }
            }
            if right >= nums.len() { None } else { Some(right) }
        }
    }

    pub fn length_of_lis(nums: Vec<i32>) -> i32 {
        if nums.is_empty() { return 0; }
        let mut dp = vec![nums[0]];
        for num in nums {
            let index = Solution::binary_search(&dp, num);
            match index {
                Some(i) => dp[i] = num,
                None => dp.push(num)
            }
        }
        dp.len() as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_length_of_lis() {
        assert_eq!(Solution::length_of_lis(vec![10,9,2,5,3,7,101,18]), 4);
    }
}