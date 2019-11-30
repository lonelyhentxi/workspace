use crate::utils::linked_list::ListNode;

/*
 * @lc app=leetcode.cn id=19 lang=rust
 *
 * [19] 删除链表的倒数第N个节点
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
impl Solution {
    pub fn remove_nth_from_end(mut head: Option<Box<ListNode>>, n: i32) -> Option<Box<ListNode>> {
        let mut current = &head;
        let mut len = 0usize;
        while let Some(node) = current {
            current = &node.next;
            len += 1;
        }
        let idx = len - n as usize;
        if idx == 0 {
            head.unwrap().next
        } else {
            let prev_idx = idx - 1;
            let mut current_idx = 0usize;
            let mut current = head.as_mut();
            while current_idx < prev_idx {
                match current {
                    Some(node) => {
                        current = node.next.as_mut();
                        current_idx +=1;
                    }
                    None=> {
                        panic!("invalid node!");
                    }
                }
            }
            current.as_mut().unwrap().next = current.as_mut().unwrap().next.as_mut().unwrap().next.take();
            head
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn returns_expected() {
        let list = ListNode::from_double_iter(vec![1,2,3,4,5].into_iter());
        let res = ListNode::from_double_iter(vec![1,2,3,5].into_iter());
        assert_eq!(Solution::remove_nth_from_end(list, 2), res);
    }
}