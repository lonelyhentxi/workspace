/*
 * @lc app=leetcode.cn id=33 lang=rust
 *
 * [33] 搜索旋转排序数组
 */

// @lc code=start
impl Solution {
    #[inline]
    fn binary_search(nums: &[i32], target: i32, mut start: isize, mut end: isize) -> i32 {
        while start <= end {
            let mid = (start + end)/2;
            if target == nums[mid as usize] {
                return mid as i32;
            } else if target < nums[mid as usize] {
                end = mid - 1;
            } else {
                start = mid + 1;
            }
        }
        -1
    }

    #[allow(clippy::collapsible_if)]
    pub fn search(nums: Vec<i32>, target: i32) -> i32 {
        if nums.is_empty() {
            return -1;
        }
        let mut start = 0isize;
        let mut end = (nums.len() - 1) as isize;
        while start <= end {
            let mid = (start + end)/2;
            let mid_v = nums[mid as usize];
            if target == mid_v {
                return mid as i32;
            };
            if mid_v > nums[end as usize] {
                if target > mid_v {
                    start = mid + 1;
                } else {
                    let may_res = Solution::binary_search(&nums, target, start, mid - 1);
                    if may_res==-1 {
                        start = mid + 1;
                    } else {
                        return may_res;
                    }
                }
            } else if mid_v < nums[start as usize] {
                if target < mid_v {
                    end = mid - 1;
                } else {
                    let may_res = Solution::binary_search(&nums, target, mid + 1, end);
                    if may_res==-1 {
                        end = mid - 1;
                    } else {
                        return may_res;
                    }
                }
            } else {
                return Solution::binary_search(&nums, target, start, end);
            }
        }
        -1
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;
    #[test]
    fn returns_expected() {
        assert_eq!(Solution::search(vec![4,5,6,7,0,1,2],0),4);
        assert_eq!(Solution::search(vec![4,5,6,7,0,1,2],3),-1);
        assert_eq!(Solution::search(vec![1],1),0);
    }

    #[test]
    fn returns_expected_long() {
        let mut long_array = (100..1_000).collect::<Vec<i32>>();
        long_array.extend((0..100).collect::<Vec<i32>>());
        assert_eq!(Solution::search(long_array.clone(), 0), 900);
        assert_eq!(Solution::search(long_array.clone(), 100), 0);
    }
}
