/*
 * @lc app=leetcode.cn id=85 lang=rust
 *
 * [85] 最大矩形
 */

// @lc code=start
impl Solution {
    fn largest_rectangle_area(heights: &mut Vec<i32>) -> i32 {
        let mut res = 0usize;
        let mut stack = vec![];
        heights.push(0);
        let mut i = 0i32;
        while i < heights.len() as i32 {
            if stack.is_empty() || heights[*stack.last().unwrap() as usize] < heights[i as usize] { 
                stack.push(i)
            } else {
                let current = stack.pop().unwrap();
                res = std::cmp::max(res, (heights[current as usize] as usize) * (if stack.is_empty() { i } else { i - stack.last().unwrap() - 1 }) as usize);
                i-=1;
            }
            i+=1;
        }
        heights.pop();
        res as i32
    }

    #[allow(clippy::needless_range_loop)]
    pub fn maximal_rectangle(matrix: Vec<Vec<char>>) -> i32 {
        let mut max_area = 0;
        if matrix.is_empty() {
            return 0;
        }
        let mut heights = vec![0;matrix[0].len()];
        for i in 0..matrix.len() {
            for j in 0..matrix[i].len() {
                heights[j] = if matrix[i][j]=='1' { heights[j] + 1 } else { 0 };
            }
            max_area = std::cmp::max(max_area, Solution::largest_rectangle_area(&mut heights));
        }
        max_area
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        let matrix = vec![
            vec!['1','0','1','0','0'],
            vec!['1','0','1','1','1'],
            vec!['1','1','1','1','1'],
            vec!['1','0','0','1','0']
        ];
        assert_eq!(Solution::maximal_rectangle(matrix), 6);
    }
}