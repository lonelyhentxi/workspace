/*
 * @lc app=leetcode.cn id=437 lang=rust
 *
 * [437] 路径总和 III
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
    pub fn path_sum(root: Option<Rc<RefCell<TreeNode>>>, sum: i32) -> i32 {
        Solution::path_sum_recurse(&root, vec![], sum) as i32
    }

    pub fn path_sum_recurse(root: &Option<Rc<RefCell<TreeNode>>>, mut values: Vec<i32>, sum: i32) -> usize {
        values.push(0);
        match root {
            None => 0,
            Some(node_ref) => {
                let node_ref_borrow = node_ref.borrow();
                let mut count = 0usize;
                for v in &mut values {
                    *v += node_ref_borrow.val;
                    if *v==sum {
                        count += 1;
                    }
                }
                count
                +Solution::path_sum_recurse(&node_ref_borrow.left, values.clone(), sum)
                +Solution::path_sum_recurse(&node_ref_borrow.right, values, sum)
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
    use crate::{tree_node,tree_leaf};

    #[test]
    fn test_path_sum_iii() {
        let tree = tree_node!(
            10,
            tree_node!(
                5,
                tree_node!(3, tree_leaf!(3), tree_leaf!(-2)),
                tree_node!(2, None, tree_leaf!(1))
            ),
            tree_node!(
                -3,None,tree_leaf!(11)
            )
        );
        assert_eq!(Solution::path_sum(tree, 8),3);
    }

    #[test]
    fn test_path_sum_iii1() {
        let tree = None;
        assert_eq!(Solution::path_sum(tree, 1),0);
    }

    #[test]
    fn test_path_sum_iii2() {
        let tree = tree_node!(
            2,
            tree_node!(
                -2,
                tree_node!(2, tree_leaf!(-2), None),
                None
            ),
            None
        );
        assert_eq!(Solution::path_sum(tree, 0),4);
    }
}

