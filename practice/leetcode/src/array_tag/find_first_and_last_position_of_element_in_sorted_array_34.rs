/*
 * @lc app=leetcode.cn id=34 lang=rust
 *
 * [34] 在排序数组中查找元素的第一个和最后一个位置
 */

// @lc code=start
impl Solution {
    fn first_greater_equal(nums: &[i32], target: i32) -> usize {
        let mut start = 0usize;
        let mut end = nums.len();
        while start < end {
            let mid = (start + end)/2;
            if nums[mid] < target {
                start = mid + 1;
            } else {
                end = mid;
            }
        }
        end
    }
    pub fn search_range(nums: Vec<i32>, target: i32) -> Vec<i32> {
        let start = Solution::first_greater_equal(&nums, target);
        if start == nums.len() || nums[start] != target { vec![-1i32, -1] }
        else {
            vec![start as i32, Solution::first_greater_equal(&nums, target + 1) as i32 - 1]
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_expected() {
        assert_eq!(Solution::search_range(vec![5,7,7,8,8,10], 8), vec![3,4]);
        assert_eq!(Solution::search_range(vec![5,7,7,8,8,10], 6), vec![-1,-1]);
    }
}

