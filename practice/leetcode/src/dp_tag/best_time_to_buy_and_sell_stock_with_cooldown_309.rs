/*
 * @lc app=leetcode.cn id=309 lang=rust
 *
 * [309] 最佳买卖股票时机含冷冻期
 */

// @lc code=start
use std::cmp;

/*
    state: start -> cooldown (think init state as cooldown)
    cooldown -> cooldown | buy
    buy -> buy | sell
    sell -> cooldown
 */

impl Solution {
    pub fn max_profit(prices: Vec<i32>) -> i32 {
        let n = prices.len();
        let mut profit_buy = vec![i32::min_value(); n+1];
        let mut profit_sell = vec![i32::min_value(); n+1];
        let mut profit_cooldown = vec![0; n+1];
        for i in 1..=prices.len() {
            let price = prices[i-1];
            profit_cooldown[i] = cmp::max(
                profit_cooldown[i-1], profit_sell[i-1]);
            profit_buy[i] = cmp::max(profit_cooldown[i-1] - price, profit_buy[i-1]);
            profit_sell[i] = profit_buy[i-1] + price;
        }
        cmp::max(cmp::max(profit_cooldown[n], profit_sell[n]), profit_buy[n])
    }
}
// @lc code=end

struct Solution;


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_max_profit() {
        assert_eq!(Solution::max_profit(vec![1,2,3,0,2]),3);
        assert_eq!(Solution::max_profit(vec![0]),0);
    }
}