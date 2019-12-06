/*
 * @lc app=leetcode.cn id=121 lang=rust
 *
 * [121] 买卖股票的最佳时机
 */

// @lc code=start
impl Solution {
    pub fn max_profit(prices: Vec<i32>) -> i32 {
        let mut min_price = i32::max_value();
        let mut max_profit = 0;
        for p in prices {
            if p < min_price {
                min_price = p;
            } else if p - min_price > max_profit {
                max_profit = p - min_price;
            }
        }
        max_profit
    }
}
// @lc code=end

pub struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_max_profit() {
        assert_eq!(Solution::max_profit(vec![7,1,5,3,6,4]),5);
        assert_eq!(Solution::max_profit(vec![7,6,4,3,1]),0);
    }
}
