/*
 * @lc app=leetcode.cn id=105 lang=rust
 *
 * [105] 从前序与中序遍历序列构造二叉树
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
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::rc::Rc;

impl Solution {
    #[allow(clippy::too_many_arguments)]
    fn build_tree_helper(
        preorder: &[i32],
        inorder: &[i32],
        inorder_indices: &HashMap<i32, usize>,
        preorder_indices: &HashMap<i32, usize>,
        pre_start_index: usize,
        pre_end_index: usize,
        in_start_index: usize,
    ) -> Option<Rc<RefCell<TreeNode>>> {
        if pre_end_index <= pre_start_index {
            return None;
        }
        let in_center_val = preorder[pre_start_index];
        let in_center_index = *inorder_indices.get(&in_center_val).unwrap();
        let left_len = in_center_index - in_start_index;
        let left = Solution::build_tree_helper(
            preorder,
            inorder,
            inorder_indices,
            preorder_indices,
            pre_start_index + 1,
            pre_start_index + 1 + left_len,
            in_start_index,
        );
        let right = Solution::build_tree_helper(
            preorder,
            inorder,
            inorder_indices,
            preorder_indices,
            pre_start_index + 1 + left_len,
            pre_end_index,
            in_center_index + 1,
        );
        Some(Rc::new(RefCell::new(TreeNode {
            val: in_center_val,
            left,
            right, 
        })))
    }

    pub fn build_tree(preorder: Vec<i32>, inorder: Vec<i32>) -> Option<Rc<RefCell<TreeNode>>> {
        let inorder_indices =
            HashMap::<i32, usize>::from_iter(inorder.iter().enumerate().map(|(i, v)| (*v, i)));
        let preorder_indices =
            HashMap::<i32, usize>::from_iter(preorder.iter().enumerate().map(|(i, v)| (*v, i)));
        Solution::build_tree_helper(
            &preorder,
            &inorder,
            &inorder_indices,
            &preorder_indices,
            0,
            preorder.len(),
            0,
        )
    }
}
// @lc code=end

pub struct Solution;

use crate::utils::tree::TreeNode;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_build_tree() {
        let truth = Solution::build_tree(vec![1, 2, 3], vec![3, 2, 1]);
        let expected = Some(Rc::new(RefCell::new(TreeNode {
            val: 1,
            left: Some(Rc::new(RefCell::new(TreeNode {
                val: 2,
                left: Some(Rc::new(RefCell::new(TreeNode::new(3)))),
                right: None,
            }))),
            right: None,
        })));
        assert_eq!(truth, expected);
    }

    #[test]
    fn test_build_tree1() {
        let truth = Solution::build_tree(vec![3, 9, 20, 15, 7], vec![9, 3, 15, 20, 7]);
        let expected = Some(Rc::new(RefCell::new(TreeNode {
            val: 3,
            left: Some(Rc::new(RefCell::new(TreeNode::new(9)))),
            right: Some(Rc::new(RefCell::new(TreeNode {
                val: 20,
                left: Some(Rc::new(RefCell::new(TreeNode::new(15)))),
                right: Some(Rc::new(RefCell::new(TreeNode::new(7)))),
            }))),
        })));
        assert_eq!(truth, expected);
    }
}
