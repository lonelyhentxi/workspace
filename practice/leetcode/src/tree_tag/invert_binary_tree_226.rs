/*
 * @lc app=leetcode.cn id=226 lang=rust
 *
 * [226] 翻转二叉树
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
impl Solution {
    pub fn invert_tree(root: Option<Rc<RefCell<TreeNode>>>) -> Option<Rc<RefCell<TreeNode>>> {
        match root {
            Some(node_ref) => {
                Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: node_ref.borrow().val,
                        left: Solution::invert_tree(node_ref.borrow().right.clone()),
                        right: Solution::invert_tree(node_ref.borrow().left.clone()),
                    }
                )))
            },
            None => None,
        }
    }
}
// @lc code=end
use crate::utils::tree::TreeNode;

struct Solution;

#[cfg(test)]
mod test {
    use super::*;


    #[test]
    fn test_invert_tree() {
        let tree1 = TreeNode::build_tree(vec![4,2,1,3,7,6,9], vec![1,2,3,4,6,7,9]);
        let tree2 = TreeNode::build_tree(vec![4,7,9,6,2,3,1], vec![9,7,6,4,3,2,1]);
        assert_eq!(Solution::invert_tree(tree1), tree2);
    }
}

