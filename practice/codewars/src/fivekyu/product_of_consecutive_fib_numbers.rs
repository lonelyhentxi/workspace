pub fn product_fib(prod: u64) -> (u64, u64, bool) {
    if prod==1 {
        return (1,1,true);
    } else if prod==0 {
        return (1,1,false);
    }
    let mut current_fib;
    let mut f0 =1;
    let mut f1=1;
    loop {
        current_fib = f0 + f1;
        f0 = f1;
        f1 = current_fib;
        if prod==f0*f1 {
            return (f0,f1,true);
        } else if prod<f0*f1 {
            return (f0,f1,false);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn basics_product_fib() {
        use super::*;
        assert_eq!(product_fib(4895), (55, 89, true));
        assert_eq!(product_fib(5895), (89, 144, false));
    }
}