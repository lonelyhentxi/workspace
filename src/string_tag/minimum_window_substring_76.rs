/*
 * @lc app=leetcode.cn id=76 lang=rust
 *
 * [76] 最小覆盖子串
 */

// @lc code=start
use std::collections::HashMap;
use std::str;

impl Solution {
    pub fn min_window(s: String, t: String) -> String {
        let s = s.as_bytes();
        let t = t.as_bytes();
        let mut start = 0usize;
        let mut end = 0usize;
        let mut t_hash = HashMap::<u8, i32>::new(); 
        for c in t {
            t_hash.entry(*c).and_modify(|v| *v += 1).or_insert(1);
        }
        let mut left = 0usize;
        let mut min_len = usize::max_value();
        let mut count = 0usize;
        for i in 0..s.len() {
            let c = s[i];
            t_hash.entry(c).and_modify(|v| {
                *v -=1;
                if *v >=0 {
                    count+=1;
                }
            });
            while count==t.len() {
                if min_len > i - left + 1 {
                    min_len = i - left + 1;
                    start = left;
                    end = left + min_len;
                }
                let c = s[left];
                t_hash.entry(c).and_modify(|v| {
                    *v += 1;
                    if *v > 0 {
                        count -= 1;
                    }
                });
                left += 1;
            }
        }
        str::from_utf8(&s[start..end]).unwrap().to_string()
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        assert_eq!(
            Solution::min_window("ADOBECODEBANC".to_string(), "ABC".to_string()),
            "BANC".to_string()
        )
    }
}
