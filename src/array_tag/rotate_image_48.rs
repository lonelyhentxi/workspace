/*
 * @lc app=leetcode.cn id=48 lang=rust
 *
 * [48] 旋转图像
 */

// @lc code=start
use std::mem::swap;

impl Solution {
    pub fn rotate(matrix: &mut Vec<Vec<i32>>) {
        let n = matrix.len();
        {
            for x in 0..n {
                for y in 0..x {
                    let (part1, part2) = matrix.split_at_mut(x);
                    swap(&mut part1[y][x], &mut part2[0][y]);
                }
            }
        }
        for x in matrix.iter_mut() {
            *x = x.iter().rev().cloned().collect::<Vec<_>>();
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_expected1() {
        let mut left = vec![
            vec![1,2,3],
            vec![4,5,6],
            vec![7,8,9]
          ];
        let right = vec![
            vec![7,4,1],
            vec![8,5,2],
            vec![9,6,3]
          ];
        Solution::rotate(&mut left);
        assert_eq!(left, right);
    }

    
    #[test]
    fn returns_expected2() {
        let mut left = vec![
            vec![5, 1, 9,11],
            vec![2, 4, 8,10],
            vec![13, 3, 6, 7], 
            vec![15,14,12,16]
          ];
        let right = vec![
            vec![15,13, 2, 5],
            vec![14, 3, 4, 1],
            vec![12, 6, 8, 9],
            vec![16, 7,10,11]
          ];
        Solution::rotate(&mut left);
        assert_eq!(left, right);
    }
}