/*
 * @lc app=leetcode.cn id=78 lang=rust
 *
 * [78] 子集
 */

// @lc code=start
#[allow(clippy::needless_range_loop)]
impl Solution {
    pub fn subsets(nums: Vec<i32>) -> Vec<Vec<i32>> {
        let all_lens = usize::pow(2usize, nums.len() as u32);
        let mut res = vec![vec![];all_lens];
        for i in 0..nums.len() {
            let mod_lens = usize::pow(2usize,(nums.len() - 1 - i) as u32);
            let num = nums[i];
            for j in 0..all_lens {
                if (j/mod_lens)%2==0 {
                    res[j].push(num);
                }
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
    use crate::utils::test_tools::assert_nested_equivalent;
    #[test]
    fn returns_expected() {
        let expected = vec![
            vec![3],
            vec![1],
            vec![2],
            vec![1,2,3],
            vec![1,3],
            vec![2,3],
            vec![1,2],
            vec![]
        ];
        assert_nested_equivalent(&Solution::subsets(vec![1,2,3]), &expected);
    }
}

