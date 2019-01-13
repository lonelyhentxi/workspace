use std::collections::BTreeSet;

pub fn min_value(digits: Vec<i32>) -> i32 {
    digits.iter().collect::<BTreeSet<&i32>>().iter().map(|i|i.to_string())
        .collect::<String>().parse::<i32>().unwrap()
}

pub fn min_value_opt(mut digits: Vec<i32>) -> i32 {
    digits.sort();
    digits.dedup();
    digits.into_iter().fold(0,|acc,x| acc*10+x)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn basic_test() {
        assert_eq!(min_value(vec![1, 3, 1]), 13);
        assert_eq!(min_value(vec![4, 7, 5, 7]), 457);
        assert_eq!(min_value(vec![4, 8, 1, 4]), 148);
    }
    #[test]
    fn basic_test_opt() {
        assert_eq!(min_value_opt(vec![1, 3, 1]), 13);
        assert_eq!(min_value_opt(vec![4, 7, 5, 7]), 457);
        assert_eq!(min_value_opt(vec![4, 8, 1, 4]), 148);
    }
}