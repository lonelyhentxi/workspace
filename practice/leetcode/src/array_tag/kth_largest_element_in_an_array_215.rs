/*
 * @lc app=leetcode.cn id=215 lang=rust
 *
 * [215] 数组中的第K个最大元素
 */

// @lc code=start
use std::collections::BinaryHeap;
use std::cmp::{self,Reverse};

impl Solution {
    fn partition(nums: &mut [i32], left: usize, right: usize) -> usize {
        let pivot = nums[left];
        let mut l = left + 1;
        let mut r = right;
        while l<= r {
            if nums[l] < pivot && nums[r] > pivot {
                nums.swap(l,r);
                l+=1;
                r-=1;
            }
            if nums[l] >= pivot {
                l+=1;
            }
            if nums[r] <= pivot {
                r-=1;
            }
        }
        nums.swap(left, r);
        r
    }

    // if use heap sort then time O(log(min(K,N-k)) * N) = O(N), space O(min(K,N-K))
    // if use quick sort then time O(N), space O(1)
    pub fn find_kth_largest_quicksort(nums: Vec<i32>, k: i32) -> i32 {
        let k = k as usize;
        let mut nums = nums;
        if nums.is_empty() {
            return 0;
        }
        let mut left = 0;
        let mut right = nums.len() - 1;
        loop {
            let pos = Solution::partition(&mut nums, left, right);
            if pos == k - 1 { return nums[pos]; }
            else if pos > k - 1 { right = pos - 1 }
            else {
                left = pos + 1;
            }
        }
    }

    pub fn find_kth_largest_heap(nums: Vec<i32>, k: i32) -> i32 {
        let k = k as usize;
        if nums.is_empty() {
            return 0;
        }
        let mut heap = BinaryHeap::with_capacity(k as usize);
        for n in nums {
            if heap.len() < k {
                heap.push(Reverse(n));
            } else {
                let Reverse(min) = heap.pop().unwrap();
                heap.push(Reverse(cmp::max(n,min)));
            }
        }
        let Reverse(res) = heap.pop().unwrap();
        res
    }

    pub fn find_kth_largest(nums: Vec<i32>, k: i32) -> i32 {
        Solution::find_kth_largest_heap(nums, k)
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_kth_largest() {
        assert_eq!(Solution::find_kth_largest(vec![3,2,1,5,6,4], 2), 5);
        assert_eq!(Solution::find_kth_largest(vec![3,2,3,1,2,4,5,5,6], 4), 4);
    }
}