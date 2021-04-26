use std::cmp::max;
use std::collections::HashSet;

fn choose_best_sum_recursively(start: usize, towns: i32, ls: &[i32]) -> HashSet<i32> {
    let mut res = HashSet::new();
    if towns<=0 {
        res.insert(0i32);
    } else {
        for i in start..ls.len()+1-(towns as usize) {
            let current_value = ls[i];
            let less_town_sums = choose_best_sum_recursively(i+1,towns-1,ls);
            for j in less_town_sums {
                res.insert(j+current_value);
            }
        }
    }
    res
}

pub fn choose_best_sum(max_miles: i32, towns: i32, ls: &[i32]) -> i32 {
    if (ls.len() as i32) < towns || towns<=0 || max_miles<=0 {
        return -1;
    }
    let sums = choose_best_sum_recursively(0 as usize,towns,ls);
    let mut max_with_limitation = -1;
    for i in sums {
        if i<= max_miles {
            max_with_limitation = max(max_with_limitation,i);
        }
    }
    max_with_limitation
}

#[cfg(test)]
mod tests {
    use super::*;
    fn testing(t: i32, k: i32, ls: &[i32], exp: i32) {
        assert_eq!(choose_best_sum(t, k, ls), exp)
    }

    #[test]
    fn basics_choose_best_sum() {

        let ts = &vec![50, 55, 56, 57, 58];
        testing(163, 3, ts, 163);
        let ts = &vec![50];
        testing(163, 3, ts, -1);
        let ts = &vec![91, 74, 73, 85, 73, 81, 87];
        testing(230, 3, ts, 228);
        testing(331, 2, ts, 178);

    }
}