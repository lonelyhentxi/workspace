/*
 * @lc app=leetcode.cn id=221 lang=rust
 *
 * [221] 最大正方形
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn maximal_square(matrix: Vec<Vec<char>>) -> i32 {
        let n = matrix.len();
        if n==0 {
            return 0;
        }
        let m = matrix[0].len();
        if m==0 {
            return 0;
        }
        let mut dpw = vec![vec![0usize;m];n];
        let mut dph = vec![vec![0usize;m];n];
        let mut dpa = vec![vec![0usize;m];n];
        let mut max_a = 0usize;
        for i in 0..n {
            for j in 0..m {
                let left_w = if j==0 { 0 } else { dpw[i][j-1] };
                let top_h = if i==0 { 0 } else { dph[i-1][j] }; 
                let last_a = if i==0 || j==0 { 0 } else { dpa[i-1][j-1] };
                if matrix[i][j] == '0' {
                    dpw[i][j] = 0;
                    dph[i][j] = 0;
                    dpa[i][j] = 0;
                } else { 
                    dpw[i][j] = 1 + left_w;
                    dph[i][j] = 1 + top_h;
                    dpa[i][j] = cmp::min(last_a, cmp::min(left_w, top_h)) + 1;
                    max_a = cmp::max(max_a, dpa[i][j]);
                }
            }
        }
        (max_a * max_a) as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_maximal_square() {
        assert_eq!(Solution::maximal_square(
            vec![
                vec!['1','0','1','0','0'],
                vec!['1','0','1','1','1'],
                vec!['1','1','1','1','1'],
                vec!['1','0','0','1','0'],
            ]
        ), 4);
    }
}