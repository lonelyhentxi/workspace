/*
 * @lc app=leetcode.cn id=124 lang=rust
 *
 * [124] 二叉树中的最大路径和
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
    fn max_path_sum_helper(root: &Option<Rc<RefCell<TreeNode>>>, max_sum: &mut i32) -> i32 {
        match root {
            Some(node_ref) => {
                let node = node_ref.borrow();
                let left_gain = max(Solution::max_path_sum_helper(&node.left,max_sum),0);
                let right_gain = max(Solution::max_path_sum_helper(&node.right,max_sum),0);
                let new_path_gain = node.val + left_gain + right_gain;
                *max_sum = max(*max_sum, new_path_gain);
                node.val + max(left_gain, right_gain)
            },
            None => 0,
        }
    }

    pub fn max_path_sum(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
        let mut max_sum = i32::min_value();
        Solution::max_path_sum_helper(&root, &mut max_sum);
        max_sum
    }
}
// @lc code=end

struct Solution;

use crate::utils::tree::TreeNode;


#[cfg(test)]
mod test {
    use super::*;
    use crate::tree_tag::construct_binary_tree_from_preorder_and_inorder_traversal_105 as constructor;
    #[test]
    fn test_max_path_sum1() {
        let tree1 = constructor::Solution::build_tree(vec![1,2,3],vec![2,1,3]);
        assert_eq!(Solution::max_path_sum(tree1), 6);
    }

    #[test]
    fn test_max_path_sum2() {
        let tree2 = constructor::Solution::build_tree(vec![-10,9,20,15,7],vec![9,-10,15,20,7]);
        assert_eq!(Solution::max_path_sum(tree2), 42);
    }
}
