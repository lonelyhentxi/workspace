use std::collections::{BTreeSet, HashSet};
use std::iter::FromIterator;
use std::hash::Hash;
use std::fmt::Debug;

pub fn assert_nested_equivalent<T: Eq + Ord + Hash + Debug>(left: &[Vec<T>], right: &[Vec<T>]) {
    let left = left.iter()
        .map(|x| BTreeSet::from_iter(x.iter())).collect::<HashSet<_>>();
    let right = right.iter()
        .map(|x| BTreeSet::from_iter(x.iter())).collect::<HashSet<_>>();
    assert_eq!(left, right);
}

pub fn map_to_string(strs: &[&str]) -> Vec<String> {
    strs.iter().map(|x| x.to_string()).collect()
}