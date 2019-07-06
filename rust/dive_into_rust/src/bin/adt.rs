#![feature(never_type)]
fn main() {
    {
        // 代数类型系统
        {
            // 集合的基数是 |集合|
            // 基数相同的集合携带的信息量实际上是相等的
            // tuple、struct、tuple struct 实际上只是有相同的内存布局，只是有不同的名字
        }
        {
            // Never Type
            // 储存一个类型需要的 bits_of(T) = log2(cardinality(T))
            // 空 enum 可以类比为 never type，在 rust 中 never type：
            // 在运行时根本不可能存在，
            // cardinality(Never) = 0
            // 其内存容量在逻辑上不可能存在
            // 处理这种类型的代码，根本不可能运行
            // 返回这种类型的代码，根本不可能返回
            // 它可以被转换成任意类型
            {
                // 可以使得泛型代码兼容 diverging function
                // fn call_fn<T,F:Fn(i32)->T>(f:F,arg:i32) -> T {f(arg)};
                // call_fn(std::process::exit,0); // 返回类型是 ！，否则将会出现编译错误
            }
            {
                // 更好的死代码检查
                // let t = std::thread::spawn(|| panic!("nope"));
                // t.join().unwrap();
                // println!("finish");
            }
            {
                // 可以用更好的方法表达 “不可能出现的情况”
                use std::result::Result;
                {
                    pub trait FromStr {
                        type Err;
                        fn from_str(s:&str) -> Result<Self,Self::Err> where Self: std::marker::Sized;
                    }

                    struct T(String);

                    impl FromStr for T {
                        type Err = !;
                        fn from_str(s:&str) -> Result<T,!> {
                            Ok(T(String::from(s)))
                        }
                    }
                }
                {
                    use std::str::FromStr;
                    use std::mem::{size_of, size_of_val};
                    struct T(String);
                    impl FromStr for T {
                        type Err = !;
                        fn from_str(s:&str) -> Result<T,!> {
                            Ok(T(String::from(s)))
                        }
                    }
                    let r: Result<T,!> = T::from_str("hello");
                    println!("Size of T: {}", size_of::<T>());
                    println!("Size of Result: {}", size_of_val(&r));
                    // 对于！类型，编译器可以直接不考虑！类型进行推断
                    // 哪怕只有 Ok 分支，编译器也可以判定其为“完整匹配”
                }
            }
        }
    }
    {
        // 再谈 option 类型
        {
            #[allow(dead_code)]
            pub enum MyOption<T> {
                None,
                Some(T),
            }
        }
        {
            let maybe_some_string = Some(String::from("Hello, World!"));
            let maybe_some_len = maybe_some_string.map(|s| s.len());
            assert_eq!(maybe_some_len,Some(13));
            fn sq(x:u32)->Option<u32>{Some(x*x)}
            #[allow(dead_code)]
            fn nope(_:u32)->Option<u32>{None}
            assert_eq!(Some(2).and_then(sq).and_then(sq),Some(16));
        }
        {
            use std::mem::size_of;
            println!("size of isize :{}",size_of::<isize>());
            println!("size of Option<isize> :{}",size_of::<Option<isize>>());
            // option 的开销非常小
            // 借用的、拥有所有权的指针因为不可能为空，所以 option 和原值内存大小相等
            // * 指针可能为0，因此需要两个内存位表示大小
            // 有空类型就应该是 option
            // 不要轻易使用 unwrap 方法，因为可能会 panic，最好使用 lint 工具禁止这个方法
            // 相对于裸指针，使用 option 包装的指针类型的执行效率不会降低
            // 不必担心这样的设计会产生大量的 match 语句
        }
    }
}