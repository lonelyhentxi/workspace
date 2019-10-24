/*
 * @lc app=leetcode.cn id=104 lang=rust
 *
 * [104] 二叉树的最大深度
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
use std::cmp::max;

impl Solution {
    pub fn max_depth(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
        match root {
            Some(root_node_ref) => {
                let root_node = root_node_ref.borrow();
                1 + max(
                    Solution::max_depth(root_node.left.clone()),
                    Solution::max_depth(root_node.right.clone())
                )
            },
            None => {
                0
            }
        }
    }
}
// @lc code=end


struct Solution;
use crate::utils::tree::TreeNode;

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_max_depth() {
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
        assert_eq!(Solution::max_depth(tree), 3);
    }
}
