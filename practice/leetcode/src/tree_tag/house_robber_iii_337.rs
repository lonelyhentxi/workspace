use crate::utils::tree::{TreeNode};

/*
 * @lc app=leetcode.cn id=337 lang=rust
 *
 * [337] 打家劫舍 III
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
    pub fn rob(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
        Solution::rob_recurse(&root).1
        

    }

    // return (not_rob, max_value)
    fn rob_recurse(root: &Option<Rc<RefCell<TreeNode>>>) -> (i32, i32) {
        match root {
            Some(node_ref) => {
                let node_ref_borrow = node_ref.borrow();
                let left_res = Solution::rob_recurse(&node_ref_borrow.left);
                let right_res = Solution::rob_recurse(&node_ref_borrow.right);
                let rob = left_res.0 + right_res.0;
                let not_rob = left_res.1 + right_res.1;
                (not_rob, cmp::max(rob + node_ref_borrow.val, not_rob))
            },
            None => {
                (0, 0)
            }
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    use crate::{tree_node, tree_leaf};

    #[test]
    fn test_rob1() {
        let tree = tree_node!(3, 
            tree_node!(2, None, tree_leaf!(3)),
            tree_node!(3, None, tree_leaf!(1))
        );
        assert_eq!(Solution::rob(tree), 7);
    }

    #[test]
    fn test_rob2() {
        let tree = tree_node!(
            3,
            tree_node!(4, tree_leaf!(1), tree_leaf!(3)),
            tree_node!(5, None, tree_leaf!(1))
        );
        assert_eq!(Solution::rob(tree), 9);
    }
}

