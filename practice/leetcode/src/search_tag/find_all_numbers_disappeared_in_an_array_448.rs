/*
 * @lc app=leetcode.cn id=448 lang=rust
 *
 * [448] 找到所有数组中消失的数字
 */

// @lc code=start
impl Solution {
    pub fn find_disappeared_numbers(mut nums: Vec<i32>) -> Vec<i32> {
        let mut res = vec![];
        for num in &mut nums {
            *num -= 1;
        }
        for i in 0..nums.len() {
            if nums[i] != -1 {
                let mut j = nums[i] as usize;
                while nums[j]!=-1 {
                    let temp = nums[j] as usize;
                    nums[j] = -1;
                    j = temp;
                }
            }
        }
        for (i,num) in nums.iter().enumerate() {
            if *num!=-1 {
                res.push((i+1) as i32);
            }
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
    fn test_find_disappeared_numbers() {
        assert_eq!(Solution::find_disappeared_numbers(vec![4,3,2,7,8,2,3,1]), vec![5,6]);
    }
}