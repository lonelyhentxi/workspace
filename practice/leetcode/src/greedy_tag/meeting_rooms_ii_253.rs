/*
 * @lc app=leetcode.cn id=253 lang=rust
 *
 * [253] 会议室 II
 */
// @lc code=start
use std::collections::BinaryHeap;
use std::cmp::Reverse;

impl Solution {
    pub fn min_meeting_rooms(mut intervals: Vec<Vec<i32>>) -> i32 {
        intervals.sort_by_key(|v| v[0]);
        let mut heap = BinaryHeap::<Reverse<i32>>::new();
        for p in &intervals {
            if let Some(Reverse(i)) = heap.peek() {
                if *i <= p[0] {
                    heap.pop();
                }
            }
            heap.push(Reverse(p[1]));
        }
        heap.len() as i32
    }
}

// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_min_meeting_rooms() {
        assert_eq!(Solution::min_meeting_rooms(
            vec![vec![0, 30],vec![5, 10],vec![15, 20]]
        ),2);
        assert_eq!(Solution::min_meeting_rooms(
            vec![vec![7,10],vec![2,4]]
        ),1);
        assert_eq!(Solution::min_meeting_rooms(
            vec![vec![13,15],vec![1,13], vec![6,9]]
        ), 2);
    }
}