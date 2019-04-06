pub fn count_bits(integer: i64) -> u32 {
    let mut n = integer as u32;
    let mut count = 0u32;
    while n!=0 {
        count += n%2;
        n /= 2;
    }
    count
}

pub fn count_bits_spec(integer: i64) -> u32 {
    integer.count_ones()
}

#[cfg(test)]
mod tests {
    #[test]
    fn returns_expected() {
        returns_expected_common(super::count_bits);
    }

    #[test]
    fn returns_expected_spec() {
        returns_expected_common(super::count_bits_spec);
    }

    fn returns_expected_common(count_bits: fn(i64)->u32) {
        assert_eq!(count_bits(0), 0);
        assert_eq!(count_bits(4), 1);
        assert_eq!(count_bits(7), 3);
        assert_eq!(count_bits(9), 2);
        assert_eq!(count_bits(10), 2);
    }
}