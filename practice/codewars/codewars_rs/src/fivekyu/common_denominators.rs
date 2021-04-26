extern crate num;

use num::integer::{lcm, gcd};

pub fn convert_fracts(l: Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    let denom = l.iter().fold(1,|acc,&(n,d)| lcm(d/gcd(n,d),acc));
    l.iter().map(|&(n, d)| (n * denom / d, denom)).collect::<Vec<(i64, i64)>>()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn testing(l: Vec<(i64, i64)>, exp: Vec<(i64, i64)>) {
        assert_eq!(convert_fracts(l), exp)
    }

    #[test]
    fn basics_convert_fracts() {
        testing(vec![(69, 130), (87, 1310), (3, 4)], vec![(18078, 34060), (2262, 34060), (25545, 34060)]);
        testing(vec![(690, 1300), (87, 1310), (30, 40)], vec![(18078, 34060), (2262, 34060), (25545, 34060)]);
    }
}