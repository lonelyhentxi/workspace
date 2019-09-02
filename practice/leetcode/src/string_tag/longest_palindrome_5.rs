/*
 * @lc app=leetcode.cn id=5 lang=rust
 *
 * [5] 最长回文子串
 */

// @lc code=start
use std::str;

impl Solution {
    pub fn longest_palindrome(s: String) -> String {
        if s.len() <= 1 {
            return s;
        }
        let mut t: Vec<u8> = vec![b'^', b'#'];
        for c in s.as_bytes() {
            t.push(*c);
            t.push(b'#');
        }
        t.push(b'$');
        let n = t.len();
        let mut p = vec![0; t.len()];
        let mut center = 0usize;
        let mut max_right = 0usize;
        let mut start = 0;
        let mut max_len = 1;
        for i in 1..n {
            if max_right > i {
                let mirror = 2 * center - i;
                p[i] = usize::min(max_right - i, p[mirror])
            };
            let mut left = i as isize - 1 - p[i] as isize;
            let mut right = i + 1 + p[i];
            loop {
                if left >= 0 && right < n && t[left as usize]==t[right] {
                    left-=1;
                    right+=1;
                    p[i]+=1;
                } else {
                    break;
                }
            }
            if i + p[i] > max_right {
                center = i;
                max_right = i + p[i];
            }
            if p[i] > max_len {
                max_len = p[i];
                start  = (i - max_len)/2
            }
        }
        let s = s.as_bytes();
        str::from_utf8(&s[start..start + max_len])
            .unwrap()
            .to_string()
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_predict() {
        assert_eq!(
            Solution::longest_palindrome("babad".into()),
            "bab".to_string()
        );
        assert_eq!(
            Solution::longest_palindrome("cbbd".into()),
            "bb".to_string()
        );
        assert_eq!(
            Solution::longest_palindrome("".into()),
            "".to_string()
        )
    }
}
