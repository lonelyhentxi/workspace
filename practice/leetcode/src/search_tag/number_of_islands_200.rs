/*
 * @lc app=leetcode.cn id=200 lang=rust
 *
 * [200] 岛屿数量
 */

// @lc code=start
const DIRECTIONS: &[(isize, isize)] = &[(-1, 0),(0, -1)];

struct UnionFind {
    sz: Vec<usize>,
    id: Vec<usize>,
    count: usize,
}

impl UnionFind {
    pub fn new(size: usize) -> UnionFind {
        let sz = vec![0usize; size];
        let mut id = vec![];
        for i in 0..size {
            id.push(i);
        }
        UnionFind {
            sz,
            id,
            count: size,
        }
    }

    pub fn find(&self, mut p: usize) -> usize {
        while p!= self.id[p] {
            p = self.id[p];
        }
        p
    }

    pub fn connected(&self, p: usize, q: usize) -> bool {
        self.find(p) == self.find(q)
    }

    pub fn union(&mut self, p: usize, q: usize) {
        let i = self.find(p);
        let j = self.find(q);
        if i==j { return; }
        if self.sz[i] < self.sz[j] { self.id[i] = j; self.sz[j] += self.sz[i]; }
        else { self.id[j] = i; self.sz[i] = self.sz[j]; }
        self.count-=1;
    }

    #[inline]
    pub fn count(&self) -> usize {
        self.count
    }
}

impl Solution {
    fn index(x: usize, y: usize, m: usize) -> usize {
        x * m + y
    }

    fn mask_union(grid: &[Vec<char>], uf: &mut UnionFind,
        x: usize, y: usize, m: usize
        ) {
        for d in DIRECTIONS {
            let next_x = (x as isize) + d.0;
            let next_y = (y as isize) + d.1;
            if next_x < 0 || next_y < 0 {
                continue;
            }
            let next_x = next_x as usize; 
            let next_y = next_y as usize;
            if grid[next_x][next_y]=='1' {
                uf.union(Solution::index(next_x, next_y, m), Solution::index(x, y, m));
            }
        }
    }
    
    pub fn num_islands(grid: Vec<Vec<char>>) -> i32 {
        if grid.is_empty() {
            return 0;
        }
        let n = grid.len();
        if grid[0].is_empty() {
            return 0;
        }
        let m = grid[0].len();
        let mut zeros = 0usize;
        let mut uf = UnionFind::new(m*n);
        for x in 0..n {
            for y in 0..m {
                if grid[x][y] == '0' {
                    zeros += 1;
                } else {
                    Solution::mask_union(&grid, &mut uf, x, y, m);
                }
            }
        }
        (uf.count() - zeros) as i32
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    fn str_to_grids(input: &str) -> Vec<Vec<char>> {
        input.split('\n').map(|line|{
            line.chars().collect::<Vec<char>>()
        }).collect::<Vec<_>>()
    }

    #[test]
    fn test_num_islands1() {
        let grid = str_to_grids("11110\n11010\n11000\n00000");
        assert_eq!(Solution::num_islands(grid),1);
    }
    
    #[test]
    fn test_num_islands2() {
        let grid = str_to_grids("11000\n00100\n00100\n00011");
        assert_eq!(Solution::num_islands(grid),3);
    }
}