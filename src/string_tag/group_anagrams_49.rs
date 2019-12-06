/*
 * @lc app=leetcode.cn id=49 lang=rust
 *
 * [49] 字母异位词分组
 */

// @lc code=start
use std::collections::HashMap;
use std::collections::BTreeMap;

impl Solution {
    pub fn group_anagrams(strs: Vec<String>) -> Vec<Vec<String>> {
        let mut note = HashMap::<BTreeMap<u8,usize>, Vec<String>>::new();
        for string in &strs {
            let mut counter = BTreeMap::<u8, usize>::new();
            for c in string.as_bytes() {
                *counter.entry(*c).or_insert(1) += 1;
            }
            note.entry(counter).and_modify(|v| v.push(string.clone())).or_insert_with(||vec![string.clone()]);
        }
        note.into_iter().map(|(_,v)| v).collect::<Vec<_>>()
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    use crate::utils::test_tools::{assert_nested_equivalent, map_to_string};

    #[test]
    fn returns_expected() {
        let input = map_to_string(&["eat", "tea", "tan", "ate", "nat", "bat"]);
        let output = Solution::group_anagrams(input);
        let expected = vec![
            map_to_string(&["ate","eat","tea"]),
            map_to_string(&["nat","tan"]),
            map_to_string(&["bat"])
          ];
        assert_nested_equivalent(&output, &expected);
    }
}

