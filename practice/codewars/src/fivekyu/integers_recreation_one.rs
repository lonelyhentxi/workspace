pub fn divisors(n: u64) -> Vec<u64> {
    let mut res = vec![];
    let bound = (n as f64).sqrt().floor() as u64;
    for i in 1..= bound {
        if n%i==0 {
            res.push(i);
            res.push(n/i);
        }
    }
    if bound*bound==n {
        res.pop();
    }
    res
}

pub fn is_squared_divisors_sum_num(n: u64) -> Option<(u64,u64)> {
   let squared_divisors_sum = divisors(n)
       .iter()
       .map(|item|item.pow(2))
       .sum();
    let sqrt = (squared_divisors_sum as f64)
        .sqrt()
        .floor() as u64;
    if sqrt*sqrt == squared_divisors_sum {
        Some((n,squared_divisors_sum))
    } else {
        None
    }
}

pub fn list_squared(m: u64, n: u64) -> Vec<(u64,u64)> {
    let mut res = vec![];
    for i in m..=n {
        if let Some(num) = is_squared_divisors_sum_num(i) {
            res.push(num);
        };
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    fn testing(m: u64, n: u64, exp: Vec<(u64, u64)>) {
        assert_eq!(list_squared(m, n), exp)
    }

    #[test]
    fn basics_list_squared() {

        testing(1, 250, vec![(1, 1), (42, 2500), (246, 84100)]);
        testing(1, 250, vec![(1, 1), (42, 2500), (246, 84100)]);
        testing(42, 250, vec![(42, 2500), (246, 84100)]);
        testing(300, 600, vec![]);

    }
}