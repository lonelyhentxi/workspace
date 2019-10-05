/*
 * @lc app=leetcode.cn id=84 lang=rust
 *
 * [84] 柱状图中最大的矩形
 */

// @lc code=start
impl Solution {
    pub fn largest_rectangle_area(heights: Vec<i32>) -> i32 {
        let mut res = 0usize;
        let mut heights = heights;
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
        res as i32
    }
}
// @lc code=end

struct Solution;


#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_expected() {
        assert_eq!(Solution::largest_rectangle_area(vec![2,1,5,6,2,3]),10);
        assert_eq!(Solution::largest_rectangle_area(vec![1]),1);
    }
}