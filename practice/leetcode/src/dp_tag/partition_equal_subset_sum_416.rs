/*
 * @lc app=leetcode.cn id=416 lang=rust
 *
 * [416] 分割等和子集
 */

// @lc code=start
impl Solution {
    #[allow(clippy::needless_range_loop)]
    pub fn can_partition(nums: Vec<i32>) -> bool {
        let s: i32 = nums.iter().sum();
        if s & 1==1 {
            return false;
        }
        let target = s/2;
        let mut dp = vec![false;(target+1) as usize];
        dp[0] = true;
        for num in nums {
            for j in (num..=target).rev() {
                if dp[(j-num) as usize] {
                    dp[j as usize] = dp[(j-num) as usize];
                }
            }
        }
        dp[target as usize]
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_partition_equal_subset_sum_416() {
        assert_eq!(Solution::can_partition(vec![1,5,11,5]),true);
        assert_eq!(Solution::can_partition(vec![1,2,3,5]),false);
    }
}

