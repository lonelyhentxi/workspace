use std::rc::Rc;

fn partitions(n: i64) -> Vec<Rc<Vec<i64>>> {
    if n==0 {
        let res = vec![Rc::new(vec![])];
        res
    } else {
        let mut caches:Vec<Rc<Vec<i64>>> = vec![];
        for p in partitions(n-1) {
            let mut curr = vec![1i64];
            curr.extend_from_slice(&p[..]);
            caches.push(Rc::new(curr));
            let p_len = p.len();
            if p_len!=0 && ( p_len<2 || p[1] > p[0]) {
                let mut curr = vec![p[0]+1];
                curr.extend_from_slice(&p[1..]);
                caches.push(Rc::new(curr));
            }
        }
        caches
    }
}

fn prod(lst: &Rc<Vec<i64>>) -> i64 {
    lst.iter().fold(1,|x,y| x*(*y))
}

fn range(lst: &[i64]) -> i64 {
    let min = lst.iter().fold(i64::max_value(),|x,y| i64::min(x,*y));
    let max = lst.iter().fold(0i64, |x,y| i64::max(x,*y));
    max - min
}

fn median(lst: &[i64]) -> f64 {
    let lst_len = lst.len();
    if lst_len==0 {
        return 0f64;
    }
    if lst_len%2==0 {
        (lst[lst_len/2-1] + lst[lst_len/2]) as f64/2f64
    } else {
        lst[lst_len/2] as f64
    }
}

fn average(lst: &[i64]) -> f64 {
    lst.iter().sum::<i64>() as f64/lst.len() as f64
}

pub fn part(n: i64) -> String {
    let mut prods: Vec<i64> = partitions(n).iter().map(prod).collect();
    prods.sort();
    prods.dedup();
    let r = range(&prods[..]);
    let a = average(&prods[..]);
    let m = median(&prods[..]);
    format!("Range: {} Average: {:.2} Median: {:.2}",r,a,m)
}

#[cfg(test)]
mod tests {
    use super::*;
    fn testequal(ans: &str, sol: &str) {
        assert!(ans == sol, "Expected \"{}\", got \"{}\".", sol, ans);
    }

    #[test]
    fn returns_expected() {
        testequal(&part(1), "Range: 0 Average: 1.00 Median: 1.00");
        testequal(&part(2), "Range: 1 Average: 1.50 Median: 1.50");
        testequal(&part(3), "Range: 2 Average: 2.00 Median: 2.00");
        testequal(&part(4), "Range: 3 Average: 2.50 Median: 2.50");
        testequal(&part(5), "Range: 5 Average: 3.50 Median: 3.50");
    }
}