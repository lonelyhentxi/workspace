use crate::utils::linked_list::ListNode;
/*
 * @lc app=leetcode.cn id=23 lang=rust
 *
 * [23] 合并K个排序链表
 */

// @lc code=start
// Definition for singly-linked list.
// #[derive(PartialEq, Eq, Clone, Debug)]
// pub struct ListNode {
//   pub val: i32,
//   pub next: Option<Box<ListNode>>
// }
//
// impl ListNode {
//   #[inline]
//   fn new(val: i32) -> Self {
//     ListNode {
//       next: None,
//       val
//     }
//   }
// }
use std::cmp::{Ord, PartialEq, Ordering, PartialOrd, Reverse};
use std::collections::BinaryHeap;
use std::boxed::Box;
use std::iter::DoubleEndedIterator;
use std::collections::HashMap;

#[derive(Eq, Clone, Debug)]
struct ArgNode {
    val: i32,
    idx: usize,
}


impl PartialOrd for ArgNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
         i32::partial_cmp(&self.val, &other.val)
    }
}

impl PartialEq for ArgNode {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}

impl Ord for ArgNode {
    fn cmp(&self, other: &Self) -> Ordering {
        i32::cmp(&self.val, &other.val)
    }
}

impl ListNode {
    #[inline]
    pub fn from_double_ended_iter<T>(iter: T) -> Option<Box<ListNode>>
    where
        T: DoubleEndedIterator,
        T::Item: Into<i32>,
    {
        let mut prev = None;
        for i in iter.rev() {
            let mut new_node = Box::new(ListNode{val:i.into(),next:None});
            new_node.next = prev;
            prev = Some(new_node);
        }
        prev
    }
}

impl Solution {
    #[allow(clippy::needless_range_loop)]
    pub fn merge_k_lists(lists: Vec<Option<Box<ListNode>>>) -> Option<Box<ListNode>> {
        let mut refs = HashMap::<usize, &Box<ListNode>>::new();
        let mut heap = BinaryHeap::<_>::new();
        for i in 0..lists.len() {
            if let Some(node) = &lists[i] {
                heap.push(Reverse(ArgNode {
                    val: node.val,
                    idx: i,
                }));
                refs.insert(i, node);
            }
        }
        let mut store = vec![];
        loop {
            let min = heap.pop();
            match min {
                Some(Reverse(arg)) => {
                    store.push(arg.val);
                    let idx = arg.idx;
                    if let Some(next) = &refs[&idx].next {
                        heap.push(Reverse(ArgNode { val: next.val, idx }));
                        refs.insert(idx, next);
                    }
                }
                None => {
                    break;
                }
            }
        }
        ListNode::from_double_ended_iter(store.into_iter())
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn returns_expected() {

        let l1 = ListNode::from_double_ended_iter(vec![1,4,5].into_iter());
        let l2 = ListNode::from_double_ended_iter(vec![1,3,4].into_iter());
        let l3 = ListNode::from_double_ended_iter(vec![2,6].into_iter());
        let lists = vec![l1,l2,l3];
        let res = ListNode::from_double_ended_iter(vec![1,1,2,3,4,4,5,6].into_iter());
        assert_eq!(Solution::merge_k_lists(lists), res);
    }
}
