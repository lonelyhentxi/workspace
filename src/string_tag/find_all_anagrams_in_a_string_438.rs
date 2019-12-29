/*
 * @lc app=leetcode.cn id=438 lang=rust
 *
 * [438] 找到字符串中所有字母异位词
 */

// @lc code=start
const A_USIZE: usize = 'a' as usize;
use std::collections::HashSet;

impl Solution {
    pub fn find_anagrams(s: String, p: String) -> Vec<i32> {
        let s = s.chars().collect::<Vec<char>>();
        let p = p.chars().collect::<Vec<char>>();
        let mut res = vec![];
        if p.len()>s.len() {
            return res;
        }
        let mut p_counter = [0usize;26];
        for c in &p {
            p_counter[*c as usize - A_USIZE] += 1;
        }
        let mut w_counter = [0usize;26];
        let mut i = 0usize;
        while i < p.len() {
            w_counter[s[i] as usize - A_USIZE]+=1;
            i+=1;
        }
        let mut diff = HashSet::<usize>::new();
        for i in 0..26 {
            if w_counter[i]!=p_counter[i] {
                diff.insert(i);
            }
        }
        loop {
            let start = i-p.len();
            if diff.is_empty() {
                res.push((start) as i32);
            }
            if i>=s.len() {
                break;
            } else {
                let next_char = s[i] as usize - A_USIZE;
                let start_char = s[start] as usize - A_USIZE;
                w_counter[next_char] += 1;
                w_counter[start_char] -= 1;
                if w_counter[next_char]==p_counter[next_char] {
                    diff.remove(&next_char);
                } else {
                    diff.insert(next_char);
                }
                if w_counter[start_char]==p_counter[start_char] {
                    diff.remove(&start_char);
                } else {
                    diff.insert(start_char);
                }
            }
            i+=1;
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
    fn test_find_anagrams() {
        assert_eq!(
            Solution::find_anagrams(
                "cbaebabacd".to_string(),"abc".to_string()),
            vec![0,6]
        );
    }
}