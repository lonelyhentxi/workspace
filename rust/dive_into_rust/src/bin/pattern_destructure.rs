#![feature(non_exhaustive)]
#![feature(exclusive_range_pattern)]
#![feature(core_intrinsics)]
#[allow(unreachable_patterns)]
#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(unused_mut)]
fn main() {
    {
        // introduction
        let tuple = (1_i32, false, 3f32);
        let (head, center, tail) = tuple;
    }
    {
        // match
        {
            // 需要对所有情况做完整的，无遗漏的匹配
            enum Direction {
                East,
                West,
                South,
                North,
            }
            let x = Direction::East;
            match x {
                Direction::East => println!("East"),
                _ => println!("other"),
            }
            #[non_exhaustive]
            pub enum Error {
                NotFound,
                PermissionDenied,
                ConnectionRefused,
            }
            // 添加了 no_exhaustive 标记的 match（enum）匹配必须在最后添加上 _ 表示通用型匹配才能完成
            let x = Error::NotFound;
            let y = match x {
                Error::NotFound => 1,
                Error::PermissionDenied => 2,
                Error::ConnectionRefused => 3,
                _ => 4,
            };
        }
        {
            // 下划线
            struct P(f32, f32, f32);
            let P(x, _, y) = P(1., 2., 3.);
            // 下划线是关键字，而不是标识符
            // 使用下划线时，个数要匹配，使用 .. 时，个数可以不匹配
            let (a, ..) = (1, 2, 3);
        }
        {
            // match 也是表达式
            let x = 1;
            let c = match x {
                -1 | 1 => 'A',
                0 => 'B',
                _ => 'C',
            };
            match c {
                'a'..='z' => 1,
                'A'..='Z' => 2,
                _ => 0,
            };
        }
        {
            // guards
            enum OptionalInt {
                Value(i32),
                Missing,
            }
            let x = OptionalInt::Value(5);
            match x {
                OptionalInt::Value(i) if i > 5 => println!("Got an int bigger than five!"),
                OptionalInt::Value(..) => println!("Got an int!"),
                OptionalInt::Missing => println!("No such luck!"),
            }
            // 编译器的完整无遗漏并不完美，覆盖范围也可能重叠，要考虑 unreachable 分支和匹配先后顺序
            // 的情况
        }
        {
            // 变量绑定
            let x = 1;
            match x {
                e @ 1..=5 => println!("got a range element {}",e),
                _ => println!("anything"),
            }
            fn deep_match(v:Option<Option<i32>>)->Option<i32> {
                match v {
                    Some(r @ Some(1..10)) => r,
                    _ => None,
                }
            }
            let y = Some(Some(5));
            println!("{:?}",deep_match(y));
            let z = Some(Some(100));
            println!("{:?}",deep_match(z));
            // 在使用 @ 的同时使用 | ，需要确保在每个条件上都绑定上名字
        }
        {
            // ref 和 mut
            {
                // 如果需要绑定的是被匹配对象的引用，则可以使用 ref 关键字
                let x = 5_i32;
                match x {
                    ref f => println!("Got a reference to {}",x)
                }
                // ref 关键字避免模式匹配中可能发生的所有权转移
                // ref 是模式的一部分只能出现在赋值符号的左边
                // & 是借用符号，是表达式的一部分
            }
            {
                // 和c++一样可以通过编译错误，显示错误提示来发现类型
                fn print_type_name<T>(_arg: &T) {
                    unsafe {
                        println!("{}",std::intrinsics::type_name::<T>());
                    }
                }
                let ref x = 5_i32;
                print_type_name(&x);
            }
            {
                let mut x: &mut i32;
                // mut 1: x is mutable
                // mut 2: &i32 is mut
            }
            {
                let mut x: Option<String> = Some("hello".into());
                match x {
                    Some(ref mut i) => i.push_str("world"),
                    None => println!("None"),
                }
                println!("{:?}",x);
            }
            // or
            {
                let mut x: Option<String> = Some("hello".into());
                match &mut x {
                    Some(i) => i.push_str("world"),
                    None => println!("None"),
                }
                println!("{:?}",x);
            }
        }
        {
            // if-let 和 while-let
            {
                let x = Some(1);
                if let Some(y) = x {
                    println!("Some");
                } else {
                    println!("None");
                }
            }
            {
                enum E<T> {
                    A(T),B(T),C,D,E,F
                }
                // 支持或的最简模式匹配编译器尚未实现
                // 等价于
                let x = E::A(true);
                let r = match x {
                    E::C|E::D => 1,
                    _ => 2,
                };
                println!("{}",r);
                // while-let 同理
            }
        }
        {
            // 函数和闭包参数做模式解构
            struct T {
                item1: char,
                item2: bool,
            }
            fn test(T{item1: arg1,item2:arg2}:T) {
                println!("{} {}",arg1,arg2);
            }
            let x = T {
                item1: 'A',
                item2: true,
            };
            test(x);
        }
    }
}