/*
 * @lc app=leetcode.cn id=15 lang=rust
 *
 * [15] 三数之和
 */

// @lc code=start
use std::collections::HashSet;

impl Solution {
    pub fn three_sum(nums: Vec<i32>) -> Vec<Vec<i32>> {
        let mut nums = nums;
        let mut res = HashSet::<Vec<i32>>::new();
        nums.sort();
        if nums.is_empty() || *nums.last().unwrap() < 0 || *nums.first().unwrap() > 0 { return vec![];  }
        let end = if nums.len() as i32 - 2 < 0 { 0 } else { nums.len() - 2 };
        for k in 0..end {
            let num_k = nums[k];
            if num_k > 0 { break; }
            let target = 0 - nums[k];
            let mut i = k + 1;
            let mut j = nums.len() - 1;
            while i < j {
                match nums[i] + nums[j] {
                    sum if sum == target => {
                        res.insert(vec![num_k, nums[i], nums[j]]);
                        while i < j && nums[i] == nums[i+1] { i+=1; };
                        while i < j && nums[j] == nums[j-1] { j-=1 };
                        i+=1; 
                        j-=1; 
                    }
                    sum if sum > target => {
                        j-=1;
                    }
                    _ => {
                        i+=1;
                    }
                };
            }
        }
        res.into_iter().collect::<Vec<Vec<i32>>>()
    }
}
// @lc code=end


struct Solution;

#[cfg(test)]
mod test  {
    use super::Solution;
    use std::collections::HashSet;
    use std::iter::FromIterator;
    #[test]
    fn returns_expected() {
        assert_eq!(
            HashSet::<Vec<i32>>::from_iter(Solution::three_sum(vec![-1, 0, 1, 2, -1, -4]).into_iter()),
            HashSet::<Vec<i32>>::from_iter(vec![
                vec![-1, 0, 1],
                vec![-1, -1, 2]
              ].into_iter())
            );
    }
}
