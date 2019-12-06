/*
 * @lc app=leetcode.cn id=17 lang=rust
 *
 * [17] 电话号码的字母组合
 */

// @lc code=start
const ZERO: usize = b'0' as usize;

impl Solution {
    

    fn dfs(dict: &[&[u8]], res: &mut Vec<Vec<u8>>, temp: Vec<u8>, digits: &[u8], i: usize) {
        if i==digits.len() {
            res.push(temp);
        } else {
            for l in dict[digits[i] as usize - ZERO] {
                let mut temp = temp.clone();
                temp.push(*l);
                Solution::dfs(&dict, res, temp, digits, i+1);
            }
        }
    }

    pub fn letter_combinations(digits: String) -> Vec<String> {
        let dict: Vec<& 'static[u8]> = vec![
        b"", b"", b"abc", b"def", b"ghi", b"jkl", b"mno",b"pqrs", b"tuv", b"wxyz"];
            let mut res: Vec<Vec<u8>>= vec![];
            if !digits.is_empty() {
                Solution::dfs(&dict, &mut res, vec![], digits.as_bytes(), 0);
            }
            res.into_iter()
                .map(|string| String::from_utf8(string).unwrap())
                .collect::<Vec<_>>()
    }
}
// @lc code=end

struct Solution;


#[cfg(test)]
mod tests {
    use super::Solution;
    #[test]
    fn returns_expected() {
        let res = vec!["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"].iter().map(|s| s.to_string()).collect::<Vec<_>>();
        assert_eq!(Solution::letter_combinations("23".to_string()), res);
    }
}