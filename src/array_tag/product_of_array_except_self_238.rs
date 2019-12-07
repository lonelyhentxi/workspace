/*
 * @lc app=leetcode.cn id=238 lang=rust
 *
 * [238] 除自身以外数组的乘积
 */

// @lc code=start
impl Solution {
    pub fn product_except_self(nums: Vec<i32>) -> Vec<i32> {
        let mut res = Vec::with_capacity(nums.len());
        let mut product = 1;
        for n in &nums {
            res.push(product);
            product *= n;
        }
        product = 1;
        for i in (0..nums.len()).rev() {
            res[i] *= product;
            product *= nums[i];
        }
        res
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_product_except_self() {
        assert_eq!(Solution::product_except_self(vec![1,2,3,4]), vec![24,12,8,6]);
    }
}