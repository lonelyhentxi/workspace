/*
 * @lc app=leetcode.cn id=102 lang=rust
 *
 * [102] 二叉树的层次遍历
 */

// @lc code=start
// Definition for a binary tree node.
// #[derive(Debug, PartialEq, Eq)]
// pub struct TreeNode {
//   pub val: i32,
//   pub left: Option<Rc<RefCell<TreeNode>>>,
//   pub right: Option<Rc<RefCell<TreeNode>>>,
// }
// 
// impl TreeNode {
//   #[inline]
//   pub fn new(val: i32) -> Self {
//     TreeNode {
//       val,
//       left: None,
//       right: None
//     }
//   }
// }
use std::rc::Rc;
use std::cell::RefCell;

use std::collections::LinkedList;

impl Solution {
    pub fn level_order(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<Vec<i32>> {
        let mut res = vec![];
        let mut deque = LinkedList::<(Rc<RefCell<TreeNode>>,usize)>::new();
        if let Some(root_node_ref) = root {
            deque.push_back((root_node_ref,0usize));
        }
        while let Some(node_info) = deque.pop_front() {
            let node = node_info.0.borrow();
            let level = node_info.1;
            if level==res.len() {
                res.push(vec![node.val])
            } else {
                res[level].push(node.val);
            }
            if let Some(node_left) = &node.left {
                deque.push_back((node_left.clone(),level+1));
            }
            if let Some(node_right) = &node.right {
                deque.push_back((node_right.clone(),level+1));
            }
        }
        res
    }
}
// @lc code=end

struct Solution;
use crate::utils::tree::TreeNode;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_level_order() {
        let tree = Some(Rc::new(RefCell::new(
            TreeNode {
                val: 3,
                left: Some(Rc::new(RefCell::new(
                    TreeNode::new(9),
                ))),
                right: Some(Rc::new(RefCell::new(
                    TreeNode{
                        val: 20,
                        left: Some(Rc::new(RefCell::new(
                            TreeNode::new(15),
                        ))),
                        right: Some(Rc::new(RefCell::new(
                            TreeNode::new(7),
                        ))),
                    }
                )))
            }
        )));
        let res = vec![
            vec![3],
            vec![9,20],
            vec![15,7]
          ];
        assert_eq!(Solution::level_order(tree), res);
    }
}

