use std::cmp::Ordering::*;

pub fn number(bus_stops: &[(i32, i32)]) -> i32 {
    bus_stops
        .iter()
        .map(|&(in_num, out_num)| {
            in_num - out_num
        })
        .fold(0i32, |acc, add_num| {
            let new_acc = acc + add_num;
            match new_acc.cmp(&0i32) {
                Less => panic!("There are {} people, incredible!", new_acc),
                _ => new_acc
            }
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn returns_expected() {
        assert_eq!(number(&[(10, 0), (3, 5), (5, 8)]), 5);
        assert_eq!(number(&[(3, 0), (9, 1), (4, 10), (12, 2), (6, 1), (7, 10)]), 17);
        assert_eq!(number(&[(3, 0), (9, 1), (4, 8), (12, 2), (6, 1), (7, 8)]), 21);
    }
}