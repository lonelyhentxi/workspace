/*
 * @lc app=leetcode.cn id=96 lang=rust
 *
 * [96] 不同的二叉搜索树
 */

// @lc code=start
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

impl Solution {
    pub fn num_trees(n: i32) -> i32 {
        Solution::num_trees_with_mem(Rc::new(RefCell::new(HashMap::new())), n)
    }

    #[allow(clippy::range_minus_one)]
    fn num_trees_with_mem(mem: Rc<RefCell<HashMap<i32, i32>>>, n: i32) -> i32 {
        let mut count = 0;
        if n <= 1 {
            return 1;
        }
        if let Some(val) = mem.borrow().get(&n) {
            return *val;
        }
        for l in 0..=n - 1 {
            let r = n - 1 - l;
            count += Solution::num_trees_with_mem(mem.clone(), l)
                * Solution::num_trees_with_mem(mem.clone(), r);
        }
        mem.borrow_mut().insert(n, count);
        count
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn returns_expected() {
        assert_eq!(Solution::num_trees(3),5);
    }
}
