/*
 * @lc app=leetcode.cn id=461 lang=rust
 *
 * [461] 汉明距离
 */

// @lc code=start
impl Solution {
    pub fn hamming_distance(x: i32, y: i32) -> i32 {
        let z  = x ^ y;
        z.count_ones() as i32
    }
}
// @lc code=end

struct Solution;


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_hamming_distance() {
        assert_eq!(Solution::hamming_distance(1,4),2);
    }
}
