/*
 * @lc app=leetcode.cn id=101 lang=rust
 *
 * [101] 对称二叉树
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

enum NodeOrVal {
    Val(i32),
    Node(Rc<RefCell<TreeNode>>),
}


impl Solution {
    pub fn is_symmetric(root: Option<Rc<RefCell<TreeNode>>>) -> bool {
        match root {
            None => true,
            Some(node_ref) => {
                let node = node_ref.borrow();
                Solution::pair_option_symmetric(&node.left, &node.right)
            }
        }
    }

    pub fn pair_symmetric(left: Rc<RefCell<TreeNode>>, right: Rc<RefCell<TreeNode>>) -> bool {
        let left_node = left.borrow();
        let right_node = right.borrow();
        if left_node.val!=right_node.val {
            return false;
        }
        Solution::pair_option_symmetric(&left_node.left, &right_node.right)&&
        Solution::pair_option_symmetric(&left_node.right, &right_node.left)
    }

    pub fn pair_option_symmetric(left: &Option<Rc<RefCell<TreeNode>>>, right: &Option<Rc<RefCell<TreeNode>>>) -> bool {
        match left {
            Some(left_ref) => {
                match right {
                    Some(right_ref) => Solution::pair_symmetric(left_ref.clone(), right_ref.clone()),
                    None => false
                }
            }
            None => right.is_none()
        }
    }
}
// @lc code=end

struct Solution;

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
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn returns_expected1() {
        let tree = Some(Rc::new(RefCell::new(
            TreeNode {
                val: 1,
                left: Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: 2,
                        left: Some(Rc::new(RefCell::new(TreeNode::new(3)))),
                        right: Some(Rc::new(RefCell::new(TreeNode::new(4)))),
                    }
                ))),
                right: Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: 2,
                        left: Some(Rc::new(RefCell::new(TreeNode::new(4)))),
                        right: Some(Rc::new(RefCell::new(TreeNode::new(3)))),
                    }
                ))),
            }
        )));
        assert!(Solution::is_symmetric(tree));
    }

    #[test]
    fn returns_expected2() {
        let tree = Some(Rc::new(RefCell::new(
            TreeNode {
                val: 1,
                left: Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: 2,
                        left: None,
                        right: Some(Rc::new(RefCell::new(TreeNode::new(3)))),
                    }
                ))),
                right: Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: 2,
                        left: None,
                        right: Some(Rc::new(RefCell::new(TreeNode::new(3)))),
                    }
                ))),
            }
        )));
        assert!(!Solution::is_symmetric(tree));
    }
}