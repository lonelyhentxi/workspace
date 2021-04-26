/*
 * @lc app=leetcode.cn id=406 lang=rust
 *
 * [406] 根据身高重建队列
 */

// @lc code=start
use std::cmp::Reverse;

impl Solution {
    pub fn reconstruct_queue(mut people: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
        people.sort_by_key(|p| (Reverse(p[0]),p[1]));
        let mut queue = vec![];
        for p in people {
            queue.insert(p[1] as usize,p);
        }
        queue
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_reconstruct_queue() {
        assert_eq!(Solution::reconstruct_queue(vec![
            vec![7,0], vec![4,4], vec![7,1], vec![5,0], vec![6,1], vec![5,2]
        ]), vec![vec![5,0], vec![7,0], vec![5,2], vec![6,1], vec![4,4], vec![7,1]]);
    }
}