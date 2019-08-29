/*
 * @lc app=leetcode.cn id=2 lang=rust
 *
 * [2] 两数相加
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
    pub fn from_double_iter<T>(iter: T) -> Option<Box<ListNode>>
    where
        T: DoubleEndedIterator,
        T::Item: Into<i32>,
    {
        let mut prev = None;
        for i in iter.rev() {
            let mut new_node = Box::new(ListNode::new(i.into()));
            new_node.next = prev;
            prev = Some(new_node);
        }
        prev
    }
}

impl Solution {
    pub fn add_two_numbers(
        l1: Option<Box<ListNode>>,
        l2: Option<Box<ListNode>>,
    ) -> Option<Box<ListNode>> {
        // forward to the end
        let mut l1_iter = ListNode { next: l1, val: -1 };
        let mut l2_iter = ListNode { next: l2, val: -1 };
        let mut carry = 0;
        let mut res = vec![];
        loop {
            let mut exist = false;
            let mut sum = carry;
            if let Some(next) = l1_iter.next {
                l1_iter = *next;
                sum += l1_iter.val;
                exist = true;
            }
            if let Some(next) = l2_iter.next {
                l2_iter = *next;
                sum += l2_iter.val;
                exist = true;
            }
            if !exist {
                if carry != 0 {
                    res.push(carry);
                }
                break;
            } else {
                carry = sum / 10;
                res.push(sum % 10);
            }
        }
        if res.is_empty() {
            None
        } else {
            ListNode::from_double_iter(res.into_iter())
        }
    }
}
// @lc code=end

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

impl ListNode {
    #[inline]
    fn new(val: i32) -> Self {
        ListNode { next: None, val }
    }
}

struct Solution;

#[cfg(test)]
mod test {
    use super::ListNode;
    #[test]
    fn returns_expected() {
        let l1 = ListNode::from_double_iter(vec![2, 4, 3].into_iter());
        let l2 = ListNode::from_double_iter(vec![5, 6, 4].into_iter());
        let l3 = ListNode::from_double_iter(vec![7, 0, 8].into_iter());
        let res = super::Solution::add_two_numbers(l1, l2);
        assert_eq!(res, l3);
    }
}
