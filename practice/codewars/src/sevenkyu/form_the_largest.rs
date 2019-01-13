const ZERO_ASCII: i32 = '0' as i32;

pub fn max_number(n: u32) -> u32 {
    let mut nums: Vec<i32> =
        n.to_string()
            .chars()
            .map(|x|
                x as i32 - ZERO_ASCII).collect::<Vec<i32>>();
    nums.sort();
    nums.reverse();
    nums.iter().map(|elem| elem.to_string())
        .collect::<String>().parse::<u32>().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_tests() {
        assert_eq!(max_number(213), 321);
        assert_eq!(max_number(7389), 9873);
        assert_eq!(max_number(63729), 97632);
        assert_eq!(max_number(566797), 977665);
        assert_eq!(max_number(17693284), 98764321);
    }
}