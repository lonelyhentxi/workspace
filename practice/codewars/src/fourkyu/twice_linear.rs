pub fn dbl_linear(n: u32) -> u32{
    let n= n as usize;
    let mut arr = vec![1];
    arr.reserve(n+1);
    let mut yi = 0;
    let mut zi = 0;
    while arr.len() <= n {
        let y = arr[yi]*2+1;
        let z = arr[zi]*3+1;
        if y < z {
            arr.push(y);
            yi+=1;
        } else if y > z {
            arr.push(z);
            zi+=1;
        } else {
            yi+=1;
        }
    }
    arr[n]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn testing(n: u32, exp: u32) {
        assert_eq!(dbl_linear(n), exp)
    }

    #[test]
    fn basics_dbl_linear() {
        testing(10, 22);
        testing(20, 57);
        testing(30, 91);
        testing(50, 175);
        testing(100, 447);
    }
}