/*
 * @lc app=leetcode.cn id=287 lang=rust
 *
 * [287] 寻找重复数
 */

// @lc code=start
impl Solution {
    pub fn find_duplicate_cycle(nums: Vec<i32>) -> i32 {
        let mut slow = 0;
        let mut fast = 0;
        loop {
            slow = nums[slow as usize];
            fast = nums[nums[fast as usize] as usize];
            if slow == fast {
                break;
            }
        }
        let mut finder = 0;
        loop {
            finder = nums[finder as usize];
            slow = nums[slow as usize];
            if finder == slow {
                break;
            }
        }
        slow
    }

    pub fn find_duplicate_binary_search(nums: Vec<i32>) -> i32 {
        let mut left = 0;
        let mut right = (nums.len() - 1) as i32;
        while left < right {
            let mid = ((left + right)/2) as i32;
            let mut count = 0;
            for num in &nums {
                if *num <= mid { count += 1; }
            }
            if count <= mid { left = mid + 1; }
            else { right = mid; }
        }
        right
    }

    pub fn find_duplicate_bits(nums: Vec<i32>) -> i32 {
        let mut res = 0;
        let n = nums.len() as i32;
        for i in 0..(std::mem::size_of::<i32>() * 8) as i32 {
            let bit: i32 = 1 << i;
            let mut count1 = 0;
            let mut count2 = 0;
            for k in 0..n {
                if (k & bit) > 0 { count1 += 1;}
                if (nums[k as usize] & bit) > 0 { count2 += 1; }
            }
            if count2 > count1 { res+= bit; }
        }
        res
    }

    pub fn find_duplicate(nums: Vec<i32>) -> i32 {
        Solution::find_duplicate_bits(nums)
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_duplicates_binary_search() {
        assert_eq!(Solution::find_duplicate_binary_search(vec![1,3,4,2,2]), 2);
        assert_eq!(Solution::find_duplicate_binary_search(vec![3,1,3,4,2]), 3);
    }

    #[test]
    fn test_find_duplicates_cycle() {
        assert_eq!(Solution::find_duplicate_cycle(vec![1,3,4,2,2]), 2);
        assert_eq!(Solution::find_duplicate_cycle(vec![3,1,3,4,2]), 3);
    }

    #[test]
    fn test_find_duplicates_bits() {
        assert_eq!(Solution::find_duplicate_bits(vec![1,3,4,2,2]), 2);
        assert_eq!(Solution::find_duplicate_bits(vec![3,1,3,4,2]), 3);
    }
}
