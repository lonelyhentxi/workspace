/*
 * @lc app=leetcode.cn id=617 lang=rust
 *
 * [617] 合并二叉树
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
    pub fn merge_trees(t1: Option<Rc<RefCell<TreeNode>>>,
        t2: Option<Rc<RefCell<TreeNode>>>) -> Option<Rc<RefCell<TreeNode>>> {
            Solution::merge_trees_recurse(&t1, &t2)
    }

    fn merge_trees_recurse(t1: &Option<Rc<RefCell<TreeNode>>>,t2: &Option<Rc<RefCell<TreeNode>>>) -> Option<Rc<RefCell<TreeNode>>> {
        let mut fake_root = None;
        match t1 {
            Some(t1_ref) => {
                match t2 {
                    Some(t2_ref) => {
                        let t1_ref_borrow = t1_ref.borrow();
                        let t2_ref_borrow = t2_ref.borrow();
                        swap(&mut fake_root,&mut Some(
                            Rc::new(RefCell::new(
                                TreeNode {
                            val: t1_ref_borrow.val + t2_ref_borrow.val,
                            left: Solution::merge_trees_recurse(&t1_ref_borrow.left, &t2_ref_borrow.left),
                            right: Solution::merge_trees_recurse(&t1_ref_borrow.right,&t2_ref_borrow.right)
                            }
                        ))))
                    },
                    None => {
                        swap(&mut fake_root,&mut t1.clone())
                    }
                }
            },
            None => swap(&mut fake_root, &mut t2.clone())
        }
        fake_root
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
    fn test_merge_trees() {
        let tree1 = tree_node!(
            1,
            tree_node!(3, tree_leaf!(5), None),
            tree_leaf!(2)
        );
        let tree2 = tree_node!(
            2,
            tree_node!(1, None, tree_leaf!(4)),
            tree_node!(3, None, tree_leaf!(7))
        );
        let expected = tree_node!(
            3,
            tree_node!(4, tree_leaf!(5), tree_leaf!(4)),
            tree_node!(5, None, tree_leaf!(7))
        );
        assert_eq!(Solution::merge_trees(tree1, tree2),expected);
    }
}