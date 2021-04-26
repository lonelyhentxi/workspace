/*
 * @lc app=leetcode.cn id=538 lang=rust
 *
 * [538] 把二叉搜索树转换为累加树
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
    pub fn convert_bst(root: Option<Rc<RefCell<TreeNode>>>) -> Option<Rc<RefCell<TreeNode>>> {
        Solution::convert_bst_recurse(&root, 0);
        root
    }

    pub fn convert_bst_recurse(root: &Option<Rc<RefCell<TreeNode>>>, sum: i32) -> i32 {
        match root {
            None => 0,
            Some(node_ref) => {
                let mut node_borrow = node_ref.borrow_mut();
                let mut res = node_borrow.val;
                let right_sum = Solution::convert_bst_recurse(&node_borrow.right, sum);
                node_borrow.val += sum + right_sum;
                res += right_sum;
                let left_sum = Solution::convert_bst_recurse(&node_borrow.left, node_borrow.val);
                res += left_sum;
                res
            }
        }
    }
}
// @lc code=end

use crate::utils::tree::TreeNode;

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    use crate::{tree_node,tree_leaf};

    #[test]
    fn test_convert_bst() {
        let tree = tree_node!(
            5,tree_leaf!(2),tree_leaf!(13)
        );
        let expected = tree_node!(18, tree_leaf!(20), tree_leaf!(13));
        assert_eq!(Solution::convert_bst(tree), expected);
    }
}