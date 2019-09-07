use std::boxed::Box;
use std::iter::DoubleEndedIterator;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
  pub val: i32,
  pub next: Option<Box<ListNode>>
}

impl ListNode {
  #[inline]
  fn new(val: i32) -> Self {
    ListNode {
      next: None,
      val
    }
  }
}

impl ListNode {
    #[inline]
    pub fn from_double_iter<T>(iter: T) -> Option<Box<ListNode>>
    where
        T: DoubleEndedIterator,
        T::Item: Into<i32>,
    {
        let mut prev = None;
        for i in iter.rev() {
            let mut new_node = Box::new(ListNode::new(i.into()));
            new_node.next = prev;
            prev = Some(new_node);
        }
        prev
    }
}

