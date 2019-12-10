/*
 * @lc app=leetcode.cn id=155 lang=rust
 *
 * [155] 最小栈
 */

// @lc code=start
use std::cmp;

struct MinStack {
    min: Vec<i32>,
    data: Vec<i32>
}


/** 
 * `&self` means the method takes an immutable reference.
 * If you need a mutable reference, change it to `&mut self` instead.
 */
impl MinStack {

    /** initialize your data structure here. */
    fn new() -> Self {
        Self {
            min: vec![],
            data: vec![],
        }
    }
    
    fn push(&mut self, x: i32) {
        self.data.push(x);
        self.min.push(cmp::min(*self.min.last()
            .unwrap_or(&i32::max_value()), x));
    }
    
    fn pop(&mut self) {
        self.data.pop();
        self.min.pop();
    }
    
    fn top(&self) -> i32 {
        *self.data.last().unwrap()
    }
    
    fn get_min(&self) -> i32 {
        *self.min.last().unwrap()
    }
}

//
// Your MinStack object will be instantiated and called as such:
// let obj = MinStack::new();
// obj.push(x);
// obj.pop();
// let ret_3: i32 = obj.top();
// let ret_4: i32 = obj.get_min();
//
// @lc code=end

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_min_stack() {
        let mut min_stack = MinStack::new();
        min_stack.push(-2);
        min_stack.push(0);
        min_stack.push(-3);
        assert_eq!(min_stack.get_min(), -3);
        min_stack.pop();
        assert_eq!(min_stack.top(),0);
        assert_eq!(min_stack.get_min(),-2);
    }
}