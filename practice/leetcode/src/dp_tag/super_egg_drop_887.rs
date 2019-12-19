/*
 * @lc app=leetcode.cn id=887 lang=rust
 *
 * [887] 鸡蛋掉落
 */

// @lc code=start
use std::rc::Rc;
use std::collections::HashMap;
use std::cmp::min;
use std::cell::RefCell;

impl Solution {
    pub fn super_egg_drop(k: i32, n: i32) -> i32 {
        let mem = Rc::new(RefCell::new(HashMap::new()));
        Solution::super_egg_drop_with_mem(mem, k, n)
    }

    pub fn super_egg_drop_with_mem(mem: Rc<RefCell<HashMap<(i32,i32),i32>>>, k: i32, n: i32) -> i32 {
        if k==1 { return n }
        if n==0 { return 0 }
        if mem.borrow().contains_key(&(k,n)) {
            return mem.borrow()[&(k,n)];
        }
        let mut res = i32::max_value();
        let mut lo = 1;
        let mut hi = n;
        while lo <= hi {
            let mid = (lo+hi) / 2;
            let broken = Solution::super_egg_drop_with_mem(mem.clone(), k-1, mid - 1);
            let not_broken = Solution::super_egg_drop_with_mem(mem.clone(),k, n - mid);
            if broken > not_broken {
                hi = mid - 1;
                res = min(res, broken + 1);
            }
            else {
                lo = mid + 1;
                res = min(res, not_broken + 1);
            }
        }
        mem.borrow_mut().insert((k,n), res);
        res
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_super_egg_drop() {
        assert_eq!(Solution::super_egg_drop(1, 2), 2);
        assert_eq!(Solution::super_egg_drop(2, 6), 3);
        assert_eq!(Solution::super_egg_drop(3, 14), 4);
    }
}