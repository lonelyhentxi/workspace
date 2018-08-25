macro_rules! say_hello {
    () => (
    println!("Hello!");
    )
}

// designator
macro_rules! create_function {
    ($func_name: ident) => (
        fn $func_name() {
        println!("You called {:?}()",stringify!($func_name));
        }
    )
}

create_function!(foo);
create_function!(bar);

macro_rules! print_result {
    ($expression:expr) => {
        println!("{:?} = {:?}",stringify!($expression),$expression);
    }
}

// macro overload

macro_rules! test {
    ($left: expr; and $right: expr) => (
            println!("{:?} and {:?} is {:?}",
            stringify!($left),
            stringify!($right),
            $left && $right);
    );
    ($left:expr; or $right:expr) => (
        println!("{:?} or {:?} is {:?}",
                 stringify!($left),
                 stringify!($right),
                 $left || $right)
    );
}

// macro repeat

macro_rules! find_min {
    ($x:expr) => {$x};
    ($x:expr,$($y:expr),+) => {
        std::cmp::min($x, find_min!($($y),+));
    };
}

// macro dry

use std::ops::{Add,Mul,Sub};

macro_rules! assert_equal_len {
    ($a:ident,$b:ident,$func:ident,$op:tt) => {
        assert!($a.len() == $b.len(),
            "{:?}: dimension mismatch: {:?} {:?} {:?}",
            stringify!($func),
            ($a.len(),),
            stringify!($op),
            ($b.len(),)
        );
    };
}

macro_rules! op {
    ($func:ident, $bound:ident,$op:tt,$method: ident) => {
        #[allow(dead_code)]
        fn $func<T: $bound<T,Output = T> + Copy>(xs: &mut Vec<T>,ys: &Vec<T>) {
            assert_equal_len!(xs,ys,$func,$op);
            for(x,y) in xs.iter_mut().zip(ys.iter()) {
                *x = $bound::$method(*x, *y);
            }
        }
    }
}

op!(add_assign, Add, +=, add);
op!(mul_assgin, Mul, *=, mul);
op!(sub_assign, Sub, -=, sub);

mod test {
    #![allow(unused_imports)]
    use std::iter;
    macro_rules! test {
        ($func:ident,$x:expr,$y:expr, $z:expr) => {
            #[test]
            fn $func() {
                for size in 0usize..10 {
                    let mut x:Vec<_> = iter::repeat($x).take(size).collect();
                    let y: Vec<_> = iter::repeat($y).take(size).collect();
                    let z: Vec<_> = iter::repeat($z).take(size).collect();
                    super::$func(&mut x,&y);
                    assert_eq!(x,z);
                }
            }
        };
    }
    test!(add_assign, 1u32, 2u32, 3u32);
    test!(mul_assign, 2u32, 3u32, 6u32);
    test!(sub_assign, 3u32, 2u32, 1u32);
}

fn main() {
    // macro_rules!
    say_hello!();
    // designator
    // block expr ident item pat path stmt tt ty
    foo();
    bar();
    print_result!(1u32+1);
    print_result!({
        let x = 1u32;
        x*x + 2*x - 1
    });
    // macro overload
    test!(1i32 + 1 == 2i32; and 2i32 * 2 == 4i32);
    test!(true; or false);
    // macro repeat
    println!("{}", find_min!(1u32));
    println!("{}", find_min!(1u32 + 2 , 2u32));
    println!("{}", find_min!(5u32, 2u32 * 3, 4u32));
}