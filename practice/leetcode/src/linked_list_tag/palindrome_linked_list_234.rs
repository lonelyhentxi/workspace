/*
 * @lc app=leetcode.cn id=234 lang=rust
 *
 * [234] 回文链表
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
// use std::mem::swap;

impl Solution {
    fn reverse(mut head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        let mut res = None;
        while let Some(node_ref) = head {
            let new_node = Some(Box::new(ListNode {
                val: node_ref.val,
                next: res,
            }));
            res = new_node;
            head = node_ref.next
        }
        res
    }

    pub fn is_palindrome(mut head: Option<Box<ListNode>>) -> bool {
        let mut count = 0usize;
        {
            let mut fast = &head;
            while let Some(node_ref) = fast {
                fast = &node_ref.next;
                count += 1;
            }
        }
        if count <=1 {
            return true;
        }
        let mut slow = &mut head;
        let mut back;
        for _ in 0..count/2-1 {
            slow = &mut slow.as_mut().unwrap().next;
        }
        if count & 1 == 1 {
            let temp = slow.as_mut().unwrap().next.take();
            back = temp.unwrap().next.take();
        } else {
            back = slow.as_mut().unwrap().next.take();
        }
        back = Solution::reverse(back);
        let mut back = &back;
        let mut front = &head;
        while !back.is_none() && !front.is_none() {
            match back {
                Some(node_ref1) => {
                    back = &node_ref1.next;
                    match front {
                        Some(node_ref2) => {
                            front = &node_ref2.next;
                            if node_ref1.val != node_ref2.val {
                                return false;
                            } 
                        },
                        None => {
                            break;
                        }
                    }
                },
                None => {
                    break;
                }
            }
        }
        back.is_none() && front.is_none()
    }
}
// @lc code=end

struct Solution;

use crate::utils::linked_list::ListNode;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_is_palindrome() {
        let list1 = ListNode::from_double_iter(vec![1,2].into_iter());
        assert!(!Solution::is_palindrome(list1));
        let list2 = ListNode::from_double_iter(vec![1,2,2,1].into_iter());
        assert!(Solution::is_palindrome(list2));
    }
}