/*
 * @lc app=leetcode.cn id=98 lang=rust
 *
 * [98] 验证二叉搜索树
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
    Node(Rc<RefCell<TreeNode>>)
}

impl Solution {
    fn inorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
        let mut res = vec![];
        let mut stack: Vec<NodeOrVal> = vec![];
        match root {
            Some(root_node) => stack.push(NodeOrVal::Node(root_node)),
            None => return vec![],
        }
        while let Some(back) = stack.pop() {
            match back {
                NodeOrVal::Node(node_ref) => {
                    let node = node_ref.borrow();
                    if let Some(right) = node.right.clone() {
                        stack.push(NodeOrVal::Node(right));
                    };
                    stack.push(NodeOrVal::Val(node.val));
                    if let Some(left) = node.left.clone() {
                        stack.push(NodeOrVal::Node(left));
                    };
                },
                NodeOrVal::Val(val) => {
                    res.push(val);
                }
            }
        }
        res
    }

    pub fn is_valid_bst(root: Option<Rc<RefCell<TreeNode>>>) -> bool {
        let traversal = Solution::inorder_traversal(root);
        for i in 1..traversal.len() {
            if traversal[i] <= traversal[i-1] {
                return false;
            }
        }
        true
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
                val: 2,
                left: Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: 1,
                        left: None,
                        right: None,
                    }
                ))),
                right: Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: 3,
                        left: None,
                        right: None,
                    }
                ))),
            }
        )));
        assert!(Solution::is_valid_bst(tree));
    }
    #[test]
    fn returns_expected2() {
        let tree = Some(Rc::new(RefCell::new(
            TreeNode {
                val: 5,
                left: Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: 1,
                        left: None,
                        right: None,
                    }
                ))),
                right: Some(Rc::new(RefCell::new(
                    TreeNode {
                        val: 4,
                        left: Some(Rc::new(RefCell::new(
                            TreeNode {
                                val: 3,
                                left: None,
                                right: None,
                            }
                        ))),
                        right:  Some(Rc::new(RefCell::new(
                            TreeNode {
                                val: 6,
                                left: None,
                                right: None,
                            }
                        ))),
                    }
                )))
            }
        )));
        assert!(!Solution::is_valid_bst(tree));
    }
}
