/*
 * @lc app=leetcode.cn id=322 lang=rust
 *
 * [322] 零钱兑换
 */

// @lc code=start
use std::cmp;

impl Solution {
    pub fn coin_change(coins: Vec<i32>, amount: i32) -> i32 {
        if amount < 0 {
            return -1;
        }
        let mut mem = vec![i32::max_value(); (amount + 1) as usize];
        mem[0] = 0;
        for i in 1..=amount {
            let mut min_coin = i32::max_value();
            for c in &coins {
                if i - c >= 0 && mem[(i-c) as usize] !=i32::max_value() {
                    min_coin = cmp::min(mem[(i-c) as usize] + 1, min_coin);
                }
            }
            mem[i as usize] = min_coin;
        }
        if mem[amount as usize]==i32::max_value() {
            -1
        } else {
            mem[amount as usize] as i32
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_coin_change() {
        assert_eq!(Solution::coin_change(vec![1,2,5], 11),3);
        assert_eq!(Solution::coin_change(vec![2], 3), -1);
    }
}