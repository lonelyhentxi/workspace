/*
 * @lc app=leetcode.cn id=114 lang=rust
 *
 * [114] 二叉树展开为链表
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
use std::mem::swap;
impl Solution {
    pub fn flatten(root: &mut Option<Rc<RefCell<TreeNode>>>) {
        match root {
            Some(node_ref) => {
                Solution::flatten(&mut node_ref.borrow_mut().left);
                Solution::flatten(&mut node_ref.borrow_mut().right);
                let mut temp = None;
                {
                    swap(&mut node_ref.borrow_mut().left, &mut temp);
                    swap(&mut temp, &mut node_ref.borrow_mut().right);
                }
                let mut current = node_ref.clone();
                let mut right = current.borrow().right.clone();
                loop {
                    match right {
                        Some(node_ref) => {
                            current = node_ref.clone();
                            right = current.borrow().right.clone();
                        },
                        None => {
                            swap(&mut current.borrow_mut().right, &mut temp);
                            break;
                        }
                    }
                }
            },
            None => {},
        }
    }
}
// @lc code=end

struct Solution;

use crate::utils::tree::TreeNode;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tree_tag::construct_binary_tree_from_preorder_and_inorder_traversal_105 as constructor;
    #[test]
    fn test_flatten() {
        let mut tree1 = constructor::Solution::build_tree(vec![1,2,3,4,5,6], vec![3,2,4,1,5,6]);
        let tree2 = constructor::Solution::build_tree(vec![1,2,3,4,5,6], vec![1,2,3,4,5,6]);
        Solution::flatten(&mut tree1);
        assert_eq!(tree1,tree2);
    }
}

