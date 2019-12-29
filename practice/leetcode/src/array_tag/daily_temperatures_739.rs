/*
 * @lc app=leetcode.cn id=739 lang=rust
 *
 * [739] 每日温度
 */

// @lc code=start
impl Solution {
    pub fn daily_temperatures(t: Vec<i32>) -> Vec<i32> {
        let mut stack: Vec<(i32, usize)> = vec![];
        let mut res = vec![0i32; t.len()];
        for (i,temp) in t.iter().enumerate().rev() {
            while let Some(l) = &stack.last() {
                if l.0 <= *temp {
                    stack.pop();
                }
                else {
                    res[i] = (l.1 - i) as i32;
                    break;
                }
            }
            stack.push((*temp,i));
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
    fn test_daily_temperatures() {
        assert_eq!(
            Solution::daily_temperatures(vec![73, 74, 75, 71, 69, 72, 76, 73]), 
            vec![1, 1, 4, 2, 1, 1, 0, 0]
        );
    }
}
