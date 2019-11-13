/*
 * @lc app=leetcode.cn id=148 lang=rust
 *
 * [148] 排序链表
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
use std::collections::BinaryHeap;

impl Solution {
    // drop one list node, then push a heap node so O(1) extra space
    pub fn sort_list(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        let mut heap = BinaryHeap::<i32>::new();
        let mut current = head;
        while let Some(node_ref) = current {
            let node = *node_ref;
            heap.push(node.val);
            current = node.next;
        }
        let mut current: Option<Box<ListNode>> = None;
        while let Some(val) = heap.pop() {
            let new_current = Some(Box::new(ListNode {
                val,
                next: current,
            }));
            current = new_current;
        }
        current
    }
}
// @lc code=end
use crate::utils::linked_list::ListNode;

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sort_list1() {
        let list1 = ListNode::from_doubled_iter(vec![4,2,1,3].into_iter());
        let list2 = ListNode::from_doubled_iter(vec![1,2,3,4].into_iter());
        assert_eq!(Solution::sort_list(list1), list2);
    }

    #[test]
    fn test_sort_list2() {
        let list1 = ListNode::from_doubled_iter(vec![-1,5,3,4,0].into_iter());
        let list2 = ListNode::from_doubled_iter(vec![-1,0,3,4,5].into_iter());
        assert_eq!(Solution::sort_list(list1), list2);
    }
}

