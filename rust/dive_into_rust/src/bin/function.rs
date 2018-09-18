#![feature(const_fn)]
#[allow(unused_mut)]
#[allow(unused_variables)]
#[allow(dead_code)]
fn main() {
    // introduction
    {
        fn add1(t: (i32, i32)) -> i32 {
            t.0 + t.1
        }
        fn add2((x, y): (i32, i32)) -> i32 {
            x + y
        }
        // 虽然不同的函数具有相同的参数和返回值类型，但是它们是不同的类型，转换成通用的 fn 类型
        let mut func = add1 as fn((i32,i32)) -> i32;
        let mut func: fn((i32,i32))->i32 = add2;
    }
    // 当需要的一些item仅仅在本函数有效时，可以仅仅在其中定义，避免污染外部环境
    // 发散函数
    {
        fn diverges() -> !{
            panic!("error"); // 函数不能正常返回
        }
        let p = if false {
            panic!("error");
        } else {
            100
        };
        // ! 类型可以和任意类型兼容
    }
    // main 函数
    {
        for arg in std::env::args() {
            println!("Arg: {}", arg);
        }
        for var in std::env::vars() {
            println!("Var: {:?}", var);
        }
        println!("has this var: {:#?}", std::env::var("CARGO"))
    }
    // const_fn
    {
        const fn cube(num: usize) -> usize {
            num*num*num
        }
        const DIM: usize = cube(2);
        const ARR: [i32; DIM] = [0;DIM];
        println!("{:?}",ARR);
    }
    // 函数递归调用
    {
        fn fib(index:u32)->u64 {
            if index==1 || index==2 {
                1
            } else {
                fib(index-1) + fib(index-2)
            }
        }
        let f8 = fib(8);
        println!("{}", f8);
    }
}