/*
 * @lc app=leetcode.cn id=75 lang=rust
 *
 * [75] 颜色分类
 */

// @lc code=start
use std::collections::HashMap;
use std::iter::FromIterator;

#[allow(clippy::needless_range_loop)]
impl Solution {
    pub fn sort_colors(nums: &mut Vec<i32>) {
        let mut buckets = HashMap::<i32,usize>
            ::from_iter(vec![(0,0usize),(1,0usize),(2,0usize)].into_iter());
        // volume 3 pairs
        for n in nums.iter() {
            buckets.entry(*n).and_modify(|v| { *v+=1; });
        }
        let mut start = 0usize;
        for i in 0..=2 {
            let end = start + *buckets.get(&i).unwrap();
            for j in start..end {
                nums[j] = i;
            }
            start = end;
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test  {
    use super::Solution;

    #[test]
    fn returns_expected() {
        let mut truth = vec![2,0,2,1,1,0];
        Solution::sort_colors(&mut truth);
        assert_eq!(truth, vec![0,0,1,1,2,2]);
    }
}

