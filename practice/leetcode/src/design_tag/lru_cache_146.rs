/*
 * @lc app=leetcode.cn id=146 lang=rust
 *
 * [146] LRU缓存机制
 */

// @lc code=start
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::swap;
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: (i32,i32),
    pub next: Option<Rc<RefCell<ListNode>>>,
    pub prev: Option<Rc<RefCell<ListNode>>>,
}

impl ListNode {
    pub fn new(val: (i32,i32)) -> Self {
        ListNode {
            next: None,
            prev: None,
            val,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LinkedList {
    pub head: Rc<RefCell<ListNode>>,
    pub tail: Rc<RefCell<ListNode>>,
    pub size: usize,
}

impl Default for LinkedList {
    fn default() -> Self {
        let head = Rc::new(RefCell::new(ListNode::new((-1,-1))));
        let tail = Rc::new(RefCell::new(ListNode::new((-1,-1))));
        head.borrow_mut().next = Some(tail.clone());
        tail.borrow_mut().prev = Some(head.clone());
        LinkedList {
            head,
            tail,
            size: 0usize,
        }
    }
}

impl LinkedList {
    pub fn insert_after(&mut self, current: Rc<RefCell<ListNode>>, next: Rc<RefCell<ListNode>>) {
        let next_next_wrapper = current.borrow().next.clone();
        match next_next_wrapper {
            Some(next_next) => {
                {
                    let mut next = next.borrow_mut();
                    next.next = Some(next_next.clone());
                    next.prev = Some(current.clone());
                }
                current.borrow_mut().next = Some(next.clone());
                next_next.borrow_mut().prev = Some(next.clone());
            }
            None => {
                panic!("can insert after tail");
            }
        }
        self.size += 1;
    }

    pub fn unlink(&mut self, current: Rc<RefCell<ListNode>>) {
        let mut next = None;
        let mut prev = None;
        swap(&mut next, &mut current.borrow_mut().next);
        swap(&mut prev, &mut current.borrow_mut().prev);
        if let Some(prev_ref) = prev.clone() {
            prev_ref.borrow_mut().next = next.clone();
        };
        if let Some(next_ref) = next.clone() {
            next_ref.borrow_mut().prev = prev.clone();
        };
        self.size -= 1;
    }
}

struct LRUCache {
    recent: LinkedList,
    dict: HashMap<i32, Rc<RefCell<ListNode>>>,
    capacity: usize,
    size: usize,
}

/**
 * `&self` means the method takes an immutable reference.
 * If you need a mutable reference, change it to `&mut self` instead.
 */
impl LRUCache {
    fn new(capacity: i32) -> LRUCache {
        let capacity = capacity as usize;
        LRUCache {
            capacity,
            recent: LinkedList::default(),
            dict: HashMap::with_capacity(capacity),
            size: 0usize,
        }
    }
    fn get(&mut self, key: i32) -> i32 {
        match self.dict.get(&key) {
            Some(node) => {
                let res = node.borrow().val;
                self.recent.unlink(node.clone());
                self.recent
                    .insert_after(self.recent.head.clone(), node.clone());
                res.1
            }
            None => -1,
        }
    }
    fn put(&mut self, key: i32, value: i32) {
        match self.dict.get_mut(&key) {
            Some(node) => {
                self.recent.unlink(node.clone());
                self.recent
                    .insert_after(self.recent.head.clone(), node.clone());
                node.borrow_mut().val.1 = value;
            }
            None => {
                if self.size >= self.capacity && self.size > 0 {
                    let target = self.recent.tail.borrow().prev.clone().unwrap();
                    self.dict.remove(&target.borrow().val.0);
                    self.recent.unlink(target);
                    self.size -=1;
                }
                if self.capacity != 0 {
                    let new_node = Rc::new(RefCell::new(ListNode::new((key,value))));
                    self.recent
                        .insert_after(self.recent.head.clone(), new_node.clone());
                    self.dict.insert(key, new_node);
                    self.size+=1;
                }
            }
        };
    }
}

//
// Your LRUCache object will be instantiated and called as such:
// let obj = LRUCache::new(capacity);
// let ret_1: i32 = obj.get(key);
// obj.put(key, value);
//
// @lc code=end

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_lru_cache() {
        let mut cache = LRUCache::new(2);
        cache.put(1, 1);
        cache.put(2, 2);
        assert_eq!(cache.get(1), 1);
        cache.put(3, 3);
        assert_eq!(cache.get(2), -1);
        cache.put(4, 4);
        assert_eq!(cache.get(1), -1);
        assert_eq!(cache.get(3), 3);
        assert_eq!(cache.get(4), 4);
    }
}
