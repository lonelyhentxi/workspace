/*
 * @lc app=leetcode.cn id=32 lang=rust
 *
 * [32] 最长有效括号
 */

// @lc code=start
impl Solution {
    fn is_valid(target: &[u8]) -> bool {
        let mut stack = vec![];
        for c in target {
            if *c == b'(' {
                stack.push(c);
            } else if stack.pop().is_none() {
                 return false;
            }
        }
        stack.is_empty() 
    }

    pub fn longest_valid_parentheses(s: String) -> i32 {
        let s = s.as_bytes();
        let mut max_len = 0usize;
        let mut start = 0usize;
        while start < s.len() - max_len {
            let mut  maybe_max_len = s.len() - start;
            if maybe_max_len%2==1 {
                maybe_max_len-=1;
            }
            for len in (max_len + 2..=maybe_max_len).rev().step_by(2) {
                if Solution::is_valid(&s[start..start+len]) {
                    max_len = len;
                    break;
                }
            }
            start += 1;
        }
        max_len as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        assert_eq!(Solution::longest_valid_parentheses("(()".to_string()),2);
        assert_eq!(Solution::longest_valid_parentheses(")()())".to_string()), 4);
    }
}

