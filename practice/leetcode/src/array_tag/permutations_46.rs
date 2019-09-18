/*
 * @lc app=leetcode.cn id=46 lang=rust
 *
 * [46] 全排列
 */

// @lc code=start
impl Solution {
    fn permute_dfs(mut res: &mut Vec<Vec<i32>>, nums: &[i32], temp: Vec<i32>,level: usize, visited: Vec<bool>) {
        if level==nums.len() {
            res.push(temp);
            return;
        }
        for i in 0..nums.len() {
            if !visited[i] {
                let num = nums[i];
                let mut new_visited = visited.clone();
                new_visited[i] = true;
                let mut new_temp = temp.clone();
                new_temp.push(num);
                Solution::permute_dfs(&mut res, &nums, new_temp, level+1, new_visited);
            }
        }
    }

    pub fn permute(nums: Vec<i32>) -> Vec<Vec<i32>> {
        let mut res = vec![];
        let visited = vec![false;nums.len()];
        Solution::permute_dfs(&mut res, &nums, vec![], 0, visited);
        res
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    use std::collections::HashSet;

    fn assert_equivalent(left: &[Vec<i32>], right: &[Vec<i32>]) {
        let left = left.iter().collect::<HashSet<_>>();
        let right = right.iter().collect::<HashSet<_>>();
        assert_eq!(left, right);
    }

    #[test]
    fn returns_expected() {
        let truth = Solution::permute(vec![1,2,3]);
        let expected = vec![
            vec![1,2,3],
            vec![1,3,2],
            vec![2,1,3],
            vec![2,3,1],
            vec![3,1,2],
            vec![3,2,1]];
        assert_eq!(&truth, &expected);
    }
}

