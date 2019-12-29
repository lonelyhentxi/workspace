/*
 * @lc app=leetcode.cn id=647 lang=rust
 *
 * [647] 回文子串
 */

// @lc code=start
impl Solution {
    #[allow(clippy::needless_range_loop)]
    pub fn count_substrings(s: String) -> i32 {
        if s.len() <= 1 {
            return s.len() as i32;
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
        }
        let mut res = 0usize;
        for i in 2..p.len()- 2 {
            if i & 1==0 {
              res += (p[i]+1)/2;
            } else {
              res += p[i]/2;
            }
        }
        res as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn test_count_substrings() {
        assert_eq!(Solution::count_substrings("abc".to_string()),3);
        assert_eq!(Solution::count_substrings("aaa".to_string()),6);
    }
}