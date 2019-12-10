/*
 * @lc app=leetcode.cn id=239 lang=rust
 *
 * [239] 滑动窗口最大值
 */

// @lc code=start
use std::collections::VecDeque;

impl Solution {
    pub fn max_sliding_window(nums: Vec<i32>, k: i32) -> Vec<i32> {
        let mut res = vec![];
        let k = k as usize;
        let mut deque = VecDeque::<usize>::new();
        for i in 0..nums.len() {
            if let Some(front) = deque.front() {
                if (*front as isize) == (i as isize - k as isize) {
                    deque.pop_front().unwrap();
                }
            }
            while let Some(back) = deque.back() {
                if nums[*back] < nums[i] {
                    deque.pop_back().unwrap();
                } else {
                    break;
                }
            }
            deque.push_back(i);
            if i>=k-1 {
                res.push(nums[*deque.front().unwrap()]);
            }
        };
        res
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_max_sliding_window() {
        assert_eq!(Solution::max_sliding_window(vec![1,3,-1,-3,5,3,6,7], 3), vec![3,3,5,5,6,7]);
    }
}

