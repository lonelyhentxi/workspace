/*
 * @lc app=leetcode.cn id=283 lang=rust
 *
 * [283] 移动零
 */

// @lc code=start
impl Solution {
    pub fn move_zeroes(nums: &mut Vec<i32>) {
        let mut zeroes = 0usize; 
        let mut i = 0usize;
        while i + zeroes < nums.len() {
            let temp = nums[i+zeroes];
            if temp == 0 {
                zeroes += 1;
            } else {
                nums[i] = temp;
                i+=1;
            }
        }
        while i<nums.len() {
            nums[i] = 0;
            i+=1;
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_move_zeroes() {
        let mut input = vec![0,1,0,3,12];
        let output = vec![1,3,12,0,0];
        Solution::move_zeroes(&mut input);
        assert_eq!(input, output);
    }
}