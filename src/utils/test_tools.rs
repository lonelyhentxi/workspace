use std::collections::{BTreeSet, HashSet};
use std::iter::FromIterator;
use std::hash::Hash;
use std::fmt::Debug;

pub fn assert_equivalent<T: Eq + Ord + Hash + Debug + Clone>(left: &[T], right: &[T]) {
    let left = left.iter().cloned().collect::<HashSet<_>>();
    let right = right.iter().cloned().collect::<HashSet<_>>();
    assert_eq!(left, right);
}

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

pub fn map_nested_to_string(strs: &[Vec<&str>]) -> Vec<Vec<String>> {
    strs.iter().map(|v| map_to_string(&v)).collect::<Vec<_>>()
}

pub fn assert_feq(p: f64, q: f64) {
    assert!(f64::abs(p-q)< 1e-64);
}

#[macro_export]
macro_rules! to_s {
  ($elem: expr) => {
    String::from( $elem )
  }
}