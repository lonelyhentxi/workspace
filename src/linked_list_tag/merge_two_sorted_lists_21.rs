use crate::utils::linked_list::ListNode;
/*
 * @lc app=leetcode.cn id=21 lang=rust
 *
 * [21] 合并两个有序链表
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
use std::boxed::Box;
use std::iter::DoubleEndedIterator;

impl ListNode {
    #[inline]
    pub fn from_doubled_iter<T>(iter: T) -> Option<Box<ListNode>>
    where
        T: DoubleEndedIterator,
        T::Item: Into<i32>,
    {
        let mut prev = None;
        for i in iter.rev() {
            let mut new_node = Box::new(ListNode{val:i.into(),next: None});
            new_node.next = prev;
            prev = Some(new_node);
        }
        prev
    }
}

impl Solution {
    pub fn merge_two_lists(l1: Option<Box<ListNode>>, l2: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        let mut pl1 = &l1;
        let mut pl2 = &l2;
        let mut store = vec![];
        let mut v1 = match pl1 {
            Some(node) => {
                pl1 = &node.next;
                node.val
            },
            None => {
                return l2;
            }
        };
        let mut v2 = match pl2 {
            Some(node) => {
                pl2 = &node.next;
                node.val
            },
            None => {
                return l1;
            }
        };
        loop {
            if v1 < v2 {
                store.push(v1);
                match pl1 {
                    Some(node) => {
                        v1 = node.val;
                        pl1 = &node.next;
                    },
                    None => {
                        store.push(v2);
                        break;
                    }
                }
            } else {
                store.push(v2);
                match pl2 {
                    Some(node) => {
                        v2 = node.val;
                        pl2 = &node.next;
                    },
                    None => {
                        store.push(v1);
                        break;
                    }
                }
            }
        }
        while let Some(node) = pl1 {
            store.push(node.val);
            pl1 = &node.next;
        }
        while let Some(node) = pl2 {
            store.push(node.val);
            pl2 = &node.next;
        }
        ListNode::from_doubled_iter(store.into_iter())
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn returns_expected() {
        let l1 = ListNode::from_doubled_iter(vec![1,2,4].into_iter());
        let l2 = ListNode::from_doubled_iter(vec![1,3,4].into_iter());
        let res = ListNode::from_doubled_iter(vec![1,1,2,3,4,4].into_iter());
        assert_eq!(Solution::merge_two_lists(l1,l2), res);
    }

    #[test]
    fn returns_expected_with_empty() {
        let l1 = ListNode::from_doubled_iter(Vec::<i32>::new().into_iter());
        let l2 = ListNode::from_doubled_iter(Vec::<i32>::new().into_iter());
        let res = ListNode::from_doubled_iter(Vec::<i32>::new().into_iter());
        assert_eq!(Solution::merge_two_lists(l1,l2), res);
    }

    
    #[test]
    fn returns_expected_with_longer() {
        let l1 = ListNode::from_doubled_iter(vec![1,3].into_iter());
        let l2 = ListNode::from_doubled_iter(vec![1,2,4,5].into_iter());
        let res = ListNode::from_doubled_iter(vec![1,1,2,3,4,5].into_iter());
        assert_eq!(Solution::merge_two_lists(l1,l2), res);
    }
}