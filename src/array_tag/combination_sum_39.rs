/*
 * @lc app=leetcode.cn id=39 lang=rust
 *
 * [39] 组合总和
 */

// @lc code=start
impl Solution {
    fn combination(candidates: &[i32], target: i32, max_i: usize) -> Vec<Vec<i32>> {
        if target < 0 {
            return vec![];
        }
        let mut res = vec![];
        for i in (0..=max_i).rev() {
            let c = candidates[i];
            if c==target {
                res.push(vec![c]);
            } else {
                let mut combs = Solution::combination(&candidates,target - c, i);
                combs.iter_mut().for_each(|comb| comb.push(c));
                res.extend(combs);
            }
        }
        res
    }
    
    pub fn combination_sum(candidates: Vec<i32>, target: i32) -> Vec<Vec<i32>> {
        let mut candidates = candidates;
        candidates.sort();
        if candidates.is_empty() {
            return vec![];
        }
        Solution::combination(&candidates, target, candidates.len() - 1)
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    use std::collections::HashSet;
    use std::collections::BTreeSet;

    fn assert_equivalent(vec1: &[Vec<i32>], vec2: &[Vec<i32>]) {
        let left = vec1.iter()
            .map(|v| v.iter().collect::<BTreeSet<_>>())
            .collect::<HashSet<_>>();
        let right = vec2.iter()
        .map(|v| v.iter().collect::<BTreeSet<_>>())
        .collect::<HashSet<_>>();
        assert_eq!(left, right);
    }

    #[test]
    fn return_expected() {
        assert_equivalent(&Solution::combination_sum(vec![2,3,6,7], 7), &[vec![7], vec![2,2,3]]);
        assert_equivalent(&Solution::combination_sum(vec![2,3,5], 8), &[vec![2,2,2,2], vec![2,3,3], vec![3,5]]);
    }
}
