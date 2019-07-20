#![feature(test)]
extern crate test;

#[cfg(test)]
mod tests {
    use super::*;
    use self::test::Bencher;
    #[test]
    fn it_works() {
        assert_eq!(gcd(2,3),1);
    }

    #[bench]
    fn big_number(b: &mut Bencher) {
        b.iter(|| gcd(12345, 67890))
    }
}

pub fn gcd(a: u64, b: u64) -> u64 {
    let (mut l,mut g) = if a<b {
        (a,b)
    } else {
        (b,a)
    };
    while l!=0 {
        let m = g % l;
        g = l;
        l = m;
    }
    return g;
}
