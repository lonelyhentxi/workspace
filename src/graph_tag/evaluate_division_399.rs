/*
 * @lc app=leetcode.cn id=399 lang=rust
 *
 * [399] 除法求值
 */
// @lc code=start
use std::collections::{HashMap,HashSet};
use std::iter::FromIterator;

struct UnionFind {
    count: usize,
    id: Vec<usize>,
    times: Vec<f64>,
}

impl UnionFind {
    fn new(count: usize) -> UnionFind {
        let mut id = Vec::with_capacity(count);
        for i in 0..count {
            id.push(i);
        }
        UnionFind {
            id,
            count,
            times: vec![1f64;count]
        }
    }

    fn find(&self, p: usize) -> (usize, f64) {
        let mut p = p;
        let mut times_to_root = 1f64;
        while p!=self.id[p] {
            times_to_root *= self.times[p];
            p=self.id[p];
        }
        (p, times_to_root)
    }

    fn connected(&self, p: usize, q: usize) -> Option<f64> {
        let (p_root, p_times) = self.find(p);
        let (q_root, q_times) = self.find(q);
        if p_root != q_root {
            None
        } else {
            Some(p_times / q_times)
        }
    }

    fn union(&mut self, p: usize, q: usize, times: f64) {
        let (p_root, p_times) = self.find(p);
        let (q_root, q_times) = self.find(q);
        if p_root==q_root {
            return;
        }
        self.times[p_root] = times * q_times / p_times;
        self.id[p_root] = q_root;
    }
}


impl Solution {
    pub fn calc_equation(equations: Vec<Vec<String>>, values: Vec<f64>, queries: Vec<Vec<String>>) -> Vec<f64> {
        let vars_map = { let mut vars = HashSet::<String>::new();
            vars.extend(queries.iter().flatten().cloned());
            vars.extend(equations.iter().flatten().cloned());
            HashMap::<String,usize>::from_iter(
                vars.into_iter().enumerate().map(|(i,v)| (v,i))
            )
        };
        let vars_existed = equations.iter().flatten()
            .map(|s| vars_map[s]).collect::<HashSet<_>>();
        let mut uf = UnionFind::new(vars_map.len());
        for i in 0..equations.len() {
            let p = vars_map[&equations[i][0]];
            let q = vars_map[&equations[i][1]];
            uf.union(p,q,values[i]);
        }
        let mut res = vec![];
        for query in queries {
            let p = vars_map[&query[0]];
            let q = vars_map[&query[1]];
            if !vars_existed.contains(&p) || !vars_existed.contains(&q) {
                res.push(-1f64);
            }
            else {
                match uf.connected(p, q) {
                    Some(value) => res.push(value),
                    None => res.push(-1f64)
                }
            }
        }
        res
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    use crate::utils::test_tools::{map_nested_to_string,assert_feq};

    #[test]
    fn test_calc_equation() {
        let truth = Solution::calc_equation(
            map_nested_to_string(&[vec!["a","b"], vec!["b","c"]]), 
            vec![2f64,3f64], 
            map_nested_to_string(&
                [vec!["a", "c"], vec!["b", "a"], vec!["a", "e"], 
                 vec!["a", "a"], vec!["x", "x"]]));
        let expected = vec![6.0, 0.5, -1.0, 1.0, -1.0];
        for i in 0..truth.len() {
            assert_feq(truth[i], expected[i]);
        }
    }
}