/*
 * @lc app=leetcode.cn id=338 lang=rust
 *
 * [338] 比特位计数
 */

// @lc code=start

impl Solution {
    pub fn count_bits_simple(num: i32) -> Vec<i32> {
        if num==0 { return vec![0]; }
        let mut res = vec![0,1];
        for i in 2..=num {
            res.push(res[(i-
                (1<<(
                    f64::from(i).log2().floor() as i32
                ))) as usize] + 1);
        }
        res
    }

    pub fn count_bits(num: i32) -> Vec<i32> {
        if num==0 { return vec![0]; }
        let mut res = vec![0,1];
        for i in 2..=num {
            let mid = (i/2) as usize;
            res.push(if i & 1 == 1 { res[mid] + 1 } else { res[mid] });
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
    fn test_count_bits() {
        assert_eq!(Solution::count_bits(2), vec![0,1,1]);
        assert_eq!(Solution::count_bits(5), vec![0,1,1,2,1,2]);
    }


    
    #[test]
    fn test_count_bits_simple() {
        assert_eq!(Solution::count_bits_simple(2), vec![0,1,1]);
        assert_eq!(Solution::count_bits_simple(5), vec![0,1,1,2,1,2]);
    }
}