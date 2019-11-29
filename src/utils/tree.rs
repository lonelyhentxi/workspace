use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
  pub val: i32,
  pub left: Option<Rc<RefCell<TreeNode>>>,
  pub right: Option<Rc<RefCell<TreeNode>>>,
}

impl TreeNode {
  #[inline]
  pub fn new(val: i32) -> Self {
    TreeNode {
      val,
      left: None,
      right: None
    }
  }

  pub fn build_tree(prefix:Vec<i32>, infix: Vec<i32>) -> Option<Rc<RefCell<TreeNode>>> {
    use crate::tree_tag::construct_binary_tree_from_preorder_and_inorder_traversal_105::Solution;
    Solution::build_tree(prefix, infix)
  }
}

