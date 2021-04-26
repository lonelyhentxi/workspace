/*
 * @lc app=leetcode.cn id=621 lang=rust
 *
 * [621] 任务调度器
 */

// @lc code=start
use std::collections::{BinaryHeap,HashMap,VecDeque};

impl Solution {
    pub fn least_interval(tasks: Vec<char>, n: i32) -> i32 {
        let mut counter = HashMap::new();
        for t in tasks {
            counter.entry(t).and_modify(|v| *v+=1).or_insert(1i32) ;
        }
        let mut heap = counter.into_iter().map(|v| v.1)
            .collect::<BinaryHeap<i32>>();
        let mut deque = VecDeque::<(i32,i32)>::new();
        let mut timer = 0;
        loop {
            while let Some(task) = &deque.front() {
                if timer > task.0 {
                    heap.push(deque.pop_front().unwrap().1);
                } else {
                    break
                }
            }
            match heap.pop() {
                Some(mut task) => {
                    task-=1;
                    if task>0 {
                        deque.push_back((timer+n,task));
                    }
                },
                None => {
                    if deque.is_empty() {
                        break;
                    }
                }
            }
            timer += 1;
        }
        timer
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_least_interval() {
        assert_eq!(Solution::least_interval(
            vec!['A','A','A','B','B','B'], 2
        ), 8);
        assert_eq!(Solution::least_interval(
            vec!['A','A','A','A','A','A','B','C','D','E','F','G'], 2
        ), 16);
    }
}
