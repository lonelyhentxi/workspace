use std::collections::{BTreeSet, HashSet};
use std::iter::FromIterator;

pub fn assert_nested_equivalent(left: &[Vec<String>], right: &[Vec<String>]) {
    let left = left.iter()
        .map(|x| BTreeSet::from_iter(x.iter())).collect::<HashSet<_>>();
    let right = right.iter()
        .map(|x| BTreeSet::from_iter(x.iter())).collect::<HashSet<_>>();
    assert_eq!(left, right);
}

pub fn map_to_string(strs: &[&str]) -> Vec<String> {
    strs.iter().map(|x| x.to_string()).collect()
}