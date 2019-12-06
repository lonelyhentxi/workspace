/*
 * @lc app=leetcode.cn id=79 lang=rust
 *
 * [79] 单词搜索
 */

// @lc code=start
use std::collections::HashSet;

impl Solution {
    pub fn exist(board: Vec<Vec<char>>, word: String) -> bool {
        let word = word.chars().collect::<Vec<_>>();
        if word.is_empty() {
            return true;
        }
        let n = board.len();
        if n==0 {
            return false;
        }
        let m = board[0].len();
        if m==0 {
            return false;
        }
        for i in 0..n {
            for j in 0..m {
                if Solution::exist_remain(&board, i as i32, j as i32, &word, m as i32, n as i32, &mut HashSet::<(i32,i32)>::new()) {
                    return true;
                }
            }
        }
        false
    }

    pub fn exist_remain(board: &[Vec<char>], i: i32, j: i32, remain: &[char], m: i32, n: i32, used: &mut HashSet<(i32,i32)>) -> bool {
        if remain[0] == board[i as usize][j as usize] {
            if remain.len() == 1 {
                true
            } else {
                let directions = [(1,0),(-1,0),(0,1),(0,-1)];
                used.insert((i,j));
                for (di, dj) in &directions {
                    let next = (i+di,j+dj);
                    if next.0 >= 0 && next.0 < n && next.1 >=0 && next.1 < m && !used.contains(&next)
                        && Solution::exist_remain(&board, next.0, next.1, &remain[1..remain.len()], m, n, used) {
                        return true;
                    }
                }
                used.remove(&(i,j));
                false
            }
        } else {
            false
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_expected() {
        let board = vec![
            vec!['A','B','C','E'],
            vec!['S','F','C','S'],
            vec!['A','D','E','E']
          ];
        assert!(
            Solution::exist(board.clone(), "ABCCED".to_string())
        );
        assert!(
            Solution::exist(board.clone(),"SEE".to_string())
        );
        assert!(
            !Solution::exist(board.clone(),"ABCB".to_string())
        );
    }
}

