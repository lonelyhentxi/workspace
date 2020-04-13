/*
 * @lc app=leetcode.cn id=4 lang=rust
 *
 * [4] 寻找两个有序数组的中位数
 */

// @lc code=start
use std::i32;
#[allow(clippy::many_single_char_names)]
#[allow(clippy::cast_lossless)]
impl Solution {
    pub fn find_median_sorted_arrays(nums1: Vec<i32>, nums2: Vec<i32>) -> f64 {
        let m = nums1.len();
        let n = nums2.len();
        let left = (m + n + 1) / 2;
        let right = (m + n + 2) / 2;
        if left == right {
            Solution::find_kth(&nums1, 0, &nums2, 0, left) as f64
        } else {
            (Solution::find_kth(&nums1, 0, &nums2, 0, left)
                + Solution::find_kth(&nums1, 0, &nums2, 0, right)) as f64
                / 2f64
        }
    }

    pub fn find_kth(nums1: &[i32], curr1: usize, nums2: &[i32], curr2: usize, nth: usize) -> i32 {
        if curr1 >= nums1.len() {
            return nums2[curr2 + nth - 1];
        }
        if curr2 >= nums2.len() {
            return nums1[curr1 + nth - 1];
        }
        if nth == 1 {
            return i32::min(nums1[curr1], nums2[curr2]);
        }
        let mid1 = if curr1 + nth / 2 - 1 < nums1.len() {
            nums1[curr1 + nth / 2 - 1]
        } else {
            i32::max_value()
        };
        let mid2 = if curr2 + nth / 2 - 1 < nums2.len() {
            nums2[curr2 + nth / 2 - 1]
        } else {
            i32::max_value()
        };
        if mid1 < mid2 {
            Solution::find_kth(nums1, curr1 + nth / 2, nums2, curr2, nth - nth / 2)
        } else {
            Solution::find_kth(nums1, curr1, nums2, curr2 + nth / 2, nth - nth / 2)
        }
    }
}
// @lc code=end

struct Solution;

#[allow(clippy::float_cmp)]
#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_prediction1() {
        let nums1 = vec![1, 3];
        let nums2 = vec![2];
        assert_eq!(Solution::find_median_sorted_arrays(nums1, nums2), 2f64);
    }

    #[test]
    fn returns_prediction2() {
        let nums1 = vec![1, 2];
        let nums2 = vec![3, 4];
        assert_eq!(Solution::find_median_sorted_arrays(nums1, nums2), 2.5f64);
    }
}
