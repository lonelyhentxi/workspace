/*
 * @lc app=leetcode.cn id=543 lang=rust
 *
 * [543] 二叉树的直径
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
use std::cmp;

impl Solution {
    pub fn diameter_of_binary_tree(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
        Solution::diameter_of_binary_tree_recurse(&root).2
    }

    pub fn diameter_of_binary_tree_recurse(
        root: &Option<Rc<RefCell<TreeNode>>>) -> (bool, i32, i32) {
        match root {
            None => (false,0,0),
            Some(node_ref) => {
                let node_borrow = node_ref.borrow();
                let (left_exist, left_max_depth, left_max_diameter) = 
                    Solution::diameter_of_binary_tree_recurse(
                        &node_borrow.left
                    );
                let (right_exist, right_max_depth, right_max_diameter) = 
                    Solution::diameter_of_binary_tree_recurse(
                    &node_borrow.right
                );
                let this_left_depth = left_max_depth + if left_exist { 1 } else { 0 };
                let this_right_depth = right_max_depth + if right_exist { 1 } else { 0 };
                let this_max_depth = cmp::max(this_left_depth, this_right_depth);
                let this_cross_diameter = this_left_depth + this_right_depth;
                let this_max_diameter = 
                    cmp::max(cmp::max(left_max_diameter, right_max_diameter),this_cross_diameter);
                (true, this_max_depth, this_max_diameter)
            }
        }
    }
}
// @lc code=end

use crate::utils::tree::TreeNode;
struct Solution;

#[cfg(test)]
mod test  {
    use super::*;
    use crate::{tree_node,tree_leaf};

    #[test]
    fn test_diameter_of_binary_tree() {
        let tree = tree_node!(
            1,
            tree_node!(2, tree_leaf!(4), tree_leaf!(5)),
            tree_leaf!(3)
        );
        assert_eq!(Solution::diameter_of_binary_tree(tree), 3);
    }
}
