/*
 * @lc app=leetcode.cn id=31 lang=rust
 *
 * [31] 下一个排列
 */

// @lc code=start
#[allow(clippy::needless_range_loop)]
impl Solution {
    pub fn next_permutation(nums: &mut Vec<i32>) {
        let len = nums.len();
        let mut start_down: Option<usize> = None;
        let mut last = i32::min_value();
        for i in (0..len).rev() {
            let num = nums[i];
            if num >= last {
                last = num;
            } else {
                start_down = Some(i);
                break;
            }
        }
        match start_down {
            Some(idx) => {
                let limit = nums[idx];
                let mut min = i32::max_value();
                let mut min_idx = idx;
                for i in idx+1..len {
                    let v = nums[i];
                    if v > limit && v < min  {
                        min = v;
                        min_idx = i;
                    }
                }
                nums[min_idx] = nums[idx];
                nums[idx] = min;
                nums.as_mut_slice()[idx+1..len].sort();
            },
            None => {
                nums.sort();
            }
        }
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::Solution;

    #[test]
    fn returns_expected() {
        let mut array1 = vec![1,2,3];
        Solution::next_permutation(&mut array1);
        assert_eq!(array1, vec![1,3,2]);
        let mut array2 = vec![3,2,1];
        Solution::next_permutation(&mut array2);
        assert_eq!(array2, vec![1,2,3]);
        let mut array3 = vec![1,1,5];
        Solution::next_permutation(&mut array3);
        assert_eq!(array3, vec![1,5,1]);
    }
}