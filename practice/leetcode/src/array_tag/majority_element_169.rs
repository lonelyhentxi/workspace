/*
 * @lc app=leetcode.cn id=169 lang=rust
 *
 * [169] 多数元素
 */

// @lc code=start
impl Solution {
    pub fn majority_element(nums: Vec<i32>) -> i32 {
        let mut curr_elem = None;
        let mut curr_count = 0;
        for n in nums {
            match curr_elem {
                Some(last) => {
                    if last == n {
                        curr_count += 1;
                    } else {
                        curr_count -= 1;
                        if curr_count==0 {
                            curr_elem = None;
                        }
                    }
                },
                None => {
                    curr_elem = Some(n);
                    curr_count += 1;
                }
            }
        }
        match curr_elem {
            Some(majority) => majority,
            None => panic!("no majority")
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_majority_element() {
        assert_eq!(Solution::majority_element(vec![3,2,3]),3);
        assert_eq!(Solution::majority_element(vec![2,2,1,1,1,2,2]),2);
    }
}