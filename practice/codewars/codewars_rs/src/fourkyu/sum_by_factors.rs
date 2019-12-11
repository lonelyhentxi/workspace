use std::cmp::max;
use std::collections::HashMap;

fn cal_primes(n:i64) -> Vec<i64> {
    let mut res = vec![];
    let mut tests: Vec<bool> = vec![false,false,true];
    tests.reserve((n+2) as usize);
    {
        let mut i = 3;
        while i<=n {
            tests.push(true);
            tests.push(false);
            i+=2;
        }
    }
    {
        let mut i = 3i64;
        while i <= n/2 {
            if tests[i as usize] {
                let mut j = i+i;
                    while j<=n {
                        tests[j as usize] = false;
                        j+=i;
                    }
            }
            i+=1;
        }
    }
    for i in 2..=n {
        if tests[i as usize] {
            res.push(i);
        }
    }
    res
}

pub fn sum_of_divided(l: Vec<i64>) -> Vec<(i64,i64)> {
    let max_of_i: i64 = l.iter().fold(0i64,|next,curr| max(next, i64::abs(*curr)));
    let mut counter = HashMap::<i64,i64>::new();
    let primes = cal_primes(max_of_i);
    for p in &primes {
        let mut sum = 0;
        let mut count = 0;
        for j in &l {
            if j%p==0 {
                sum += j;
                count+=1;
            }
        }
        if count!=0 {
            counter.insert(*p, sum);
        }
    }
    let mut res: Vec<(i64,i64)> = counter.into_iter().collect();
    res.sort_by(|a,b| a.0.cmp(&b.0));
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    fn testing(l: Vec<i64>, exp: Vec<(i64, i64)>) {
        assert_eq!(sum_of_divided(l), exp)
    }

    #[test]
    fn test_prime() {
      assert_eq!(cal_primes(2),vec![2i64]);
      assert_eq!(cal_primes(10),vec![2i64,3,5,7]);
    }

    #[test]
    fn test_sum_of_divide() {
        testing(vec![12, 15], vec![ (2, 12), (3, 27), (5, 15) ]);
        testing(vec![15,21,24,30,45], vec![ (2, 54), (3, 135), (5, 90), (7, 21) ]);
        testing(vec![15, 21, 24, 30, -45], vec![(2, 54), (3, 45), (5, 0), (7, 21)])
    }
}