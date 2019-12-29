/*
 * @lc app=leetcode.cn id=347 lang=rust
 *
 * [347] 前 K 个高频元素
 */
// @lc code=start
use std::collections::{HashMap,BinaryHeap};
impl Solution {
    pub fn top_k_frequent(nums: Vec<i32>, k: i32) -> Vec<i32> {
        let mut counter = HashMap::<i32,usize>::new();
        for n in nums {
            counter.entry(n).and_modify(|v| *v+=1).or_insert(0);
        }
        let mut heap = BinaryHeap::<(usize,i32)>::new();
        for (k,v) in counter {
            heap.push((v,k));
        }
        let mut res = vec![];
        for _ in 0..k {
            res.push(heap.pop().unwrap().1);
        }
        res
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_top_k_frequent() {
        assert_eq!(Solution::top_k_frequent(vec![1,1,1,2,2,3],2),vec![1,2]);
        assert_eq!(Solution::top_k_frequent(vec![1],1),vec![1]);
    }
}

