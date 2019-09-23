/*
 * @lc app=leetcode.cn id=56 lang=rust
 *
 * [56] 合并区间
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn merge(intervals: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
        if intervals.is_empty() {
            return vec![];
        }
        let mut intervals = intervals;
        intervals.sort_by_key(|v| v[0]);
        let mut res = vec![intervals[0].clone()];
        for v in &intervals[1..intervals.len()] {
            let last_index = res.len() - 1;
            let last = &mut res[last_index];
            if v[0] <= last[1] {
                last.as_mut_slice()[1] = cmp::max(last[1],v[1]);
            } else {
                res.push(v.clone());
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
        assert_eq!(
            Solution::merge(vec![vec![1,3],vec![2,6],vec![8,10],vec![15,18]]),
            vec![vec![1,6],vec![8,10],vec![15,18]]
         );
         assert_eq!(
             Solution::merge(vec![vec![1,4],vec![4,5]]), 
             vec![vec![1,5]]
            );
    }
}
