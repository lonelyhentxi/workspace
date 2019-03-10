// m should > 0, m<=n
fn partition_rec(n: isize, first: isize) -> isize {
    if n==0 {return 1;}
    if first==1 { return 1; }
    let mut sum = 0;
    for i in 1..=first {
        sum += partition_rec(n-i,isize::min(i,n-i));
    }
    sum
}


pub fn partitions_slow(n: isize) -> isize {
    if n < 0 {
        return 0;
    }
    partition_rec(n, n)
}

pub fn partitions(n: isize) -> isize {
    if n < 0 {
        return 0;
    }
    let n = n as usize;
    let mut caches = vec![0isize;n+1];
    caches[0] = 1isize;
    for i in 1..=n {
        for j in i..=n {
            caches[j] += caches[j-i];
        }
    }
    caches[n]
}

#[cfg(test)]
mod test_slow {
    use super::*;

    #[test]
    fn basic_test_01() {
        assert_eq!(partitions_slow(1), 1);
    }

    #[test]
    fn basic_test_05() {
        assert_eq!(partitions_slow(5), 7);
    }

    #[test]
    fn basic_test_10() {
        assert_eq!(partitions_slow(10), 42);
    }

    #[test]
    fn basic_test_25() {
        assert_eq!(partitions_slow(25), 1958);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test_01() {
        assert_eq!(partitions(1), 1);
    }

    #[test]
    fn basic_test_05() {
        assert_eq!(partitions(5), 7);
    }

    #[test]
    fn basic_test_10() {
        assert_eq!(partitions(10), 42);
    }

    #[test]
    fn basic_test_25() {
        assert_eq!(partitions(25), 1958);
    } 
}