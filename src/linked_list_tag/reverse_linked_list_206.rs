/*
 * @lc app=leetcode.cn id=206 lang=rust
 *
 * [206] 反转链表
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
use std::mem::swap;

impl Solution {
    pub fn append(mut one: Option<Box<ListNode>>, another: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        match &mut one {
            Some(one_ref) => {
                let mut temp = None;
                swap(&mut temp, &mut one_ref.next);
                swap(&mut one_ref.next,&mut Solution::append(temp, another));
                one
            },
            None => another
        }
    }

    pub fn reverse_list_recursive(mut head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        match &mut head {
            Some(head_ref) => {
                let mut temp = None;
                swap(&mut head_ref.next, &mut temp);
                Solution::append(Solution::reverse_list_recursive(temp), head)
            },
            None => None,
        }
    }

    pub fn reverse_list_loop(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        let mut current = head;
        let mut temp = None;
        while let Some(node_ref) =  current {
            let new_node = Some(Box::new(ListNode {
                val: node_ref.val,
                next: temp
            }));
            temp = new_node;
            current = node_ref.next;
        }
        temp
    }

    pub fn reverse_list(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        Solution::reverse_list_loop(head)
    }
}
// @lc code=end

struct Solution;

use crate::utils::linked_list::ListNode;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_reverse_list_loop() {
        let node1 = ListNode::from_doubled_iter(vec![1,2,3,4,5].into_iter());
        let node2 = ListNode::from_doubled_iter(vec![5,4,3,2,1].into_iter());
        assert_eq!(Solution::reverse_list_loop(node1),node2);
    }

    #[test]
    fn test_reverse_list_recursive() {
        let node1 = ListNode::from_doubled_iter(vec![1,2,3,4,5].into_iter());
        let node2 = ListNode::from_doubled_iter(vec![5,4,3,2,1].into_iter());
        assert_eq!(Solution::reverse_list_recursive(node1),node2);
    }
}
