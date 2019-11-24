/*
 * @lc app=leetcode.cn id=207 lang=rust
 *
 * [207] 课程表
 */

// @lc code=start
use std::collections::HashSet;

struct Node {
    in_edge: HashSet<usize>,
    out_edge: HashSet<usize>,
}

struct Graph {
    nodes: Vec<Node>,
}

impl Graph {
    pub fn new(num_courses: usize, prerequisites: Vec<Vec<i32>>) -> Graph {
        let mut nodes = vec![];
        for _ in 0..num_courses {
            nodes.push(Node { 
                in_edge: HashSet::new(), 
                out_edge: HashSet::new()
            });
        }
        for pre in prerequisites {
            let pout = pre[0] as usize;
            let pin = pre[1] as usize;
            nodes[pin].out_edge.insert(pout);
            nodes[pout].in_edge.insert(pin);
        }
        Graph {
            nodes,
        }
    }

    pub fn remove_node(&mut self,id: usize) -> Vec<usize> {
        let mut res = vec![];
        let iters = self.nodes[id].out_edge.iter().cloned().collect::<Vec<_>>();
        for in_id in iters {
            let in_edge = &mut self.nodes[in_id].in_edge;
            in_edge.remove(&id);
            if in_edge.is_empty() {
                res.push(in_id);
            }
        }
        self.nodes[id].out_edge.clear();
        res
    }

    pub fn remove_none_cycle(&mut self) {
        let mut zero_ins = vec![];
        for i in 0..self.nodes.len() {
            if self.nodes[i].in_edge.is_empty() {
                zero_ins.push(i);
            }
        }
        while let Some(zero_in) = zero_ins.pop() {
            zero_ins.extend(self.remove_node(zero_in));
        }
    }

    pub fn detect_none_cycle(num_courses: usize, prerequisites: Vec<Vec<i32>>) -> bool {
        let mut graph = Graph::new(num_courses, prerequisites);
        graph.remove_none_cycle();
        !graph.nodes.iter().any(|n| !n.in_edge.is_empty())
    }
}

impl Solution {
    pub fn can_finish(num_courses: i32, prerequisites: Vec<Vec<i32>>) -> bool {
        Graph::detect_none_cycle(num_courses as usize, prerequisites)
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_can_finished() {
        assert!(Solution::can_finish(2, vec![vec![1,0]]));
        assert!(!Solution::can_finish(2, vec![vec![1,0],vec![0,1]]));
    }
}
