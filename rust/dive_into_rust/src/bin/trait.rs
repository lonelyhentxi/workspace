#[allow(dead_code)]
fn main() {
    // 成员函数
    {
        {
            trait Shape {
                fn area(&self) -> f64;
            }
            trait T {
                fn method1(self: Self);
                fn method2(self: &Self);
                fn method3(self: &mut Self);
            }
            // equal to
            trait T1 {
                fn method1(self);
                fn method2(&self);
                fn method3(&mut self);
            }
            struct Circle {
                radius: f64
            }
            impl Shape for Circle {
                fn area(&self) -> f64 {
                    std::f64::consts::PI * self.radius * self.radius
                }
            }
            let c = Circle { radius: 2f64 };
            println!("The area is {}", c.area());
            impl Circle {
                // 可以看做 impl 了一个匿名 trait
                fn get_radius(&self) -> f64 {
                    self.radius
                }
            }
        }
        // 如果 trait 上已经有了方法体，那么在具体实现的时候，可以选择不用重写
        // 要重写，可以使用重新实现来 override
        {
            trait Shape {
                fn area(&self) -> f64;
            }

            trait Round {
                fn get_radius(&self) -> f64;
            }

            struct Circle {
                radius: f64,
            }

            impl Round for Circle {
                fn get_radius(&self) -> f64 {
                    self.radius
                }
            }

            impl Shape for dyn Round {
                fn area(&self) -> f64 {
                    std::f64::consts::PI * self.get_radius() * self.get_radius()
                }
            }

            let b = Box::new(Circle { radius: 4f64 }) as Box<Round>;
            println!("{}", b.area());
        }
    }
    // 静态方法
    {
        {
            struct T(i32);
            impl T {
                // 只要变量名不是 self，就不能使用 点运算符，是静态方法
                fn func(this: &Self) {
                    println!("value {}", this.0);
                }
            }
            let x = T(42);
            T::func(&x); // 这种方式是强迫静态方法接受一个参数
        }
        {
            // trait 中可以定义静态函数
            pub trait Default {
                fn default() -> Self; // 实际上担任了构造函数的职责
            }
        }
    }
    // 拓展方法
    {
        trait Double {
            fn double(&self) -> Self;
        }
        impl Double for i32 {
            fn double(&self) -> i32 { *self * 2 }
        }
        let x: i32 = 10.double();
        println!("{}", x);
        // 在声明 trait 和 impl trait 的时候，impl 块要不就和 trait 的声明在同一个 crate 中，
        // 要不就和类型的声明在同一个 crate 中
        // 如果是匿名 impl，那么这个 impl 必须和类型本身存在于同一个 crate 中
        // trait 在编译器内存大小不确定，不能当做 interface 使用
    }
    // 完整函数调用法
    {
        {
            trait Cook {
                fn start(&self);
            }

            trait Wash {
                fn start(&self);
            }

            struct Chef;

            impl Cook for Chef {
                fn start(&self) { println!("Cook::start"); }
            }

            impl Wash for Chef {
                fn start(&self) { println!("Wash::start"); }
            }

            let me = Chef;
            <Cook>::start(&me);
            <Chef as Wash>::start(&me);
        }
    }
    // trait 约束和继承
    {
        {
            use std::fmt::Debug;
            fn my_print1<T: Debug>(x: T) {
                println!("The value is {:?}.", x);
            }
            fn my_print2<T>(x: T) where T: Debug {
                println!("The value is {:?}.", x);
            }
        }
        {
            trait Base {};
            trait Derived: Base {};
            #[derive(Debug)] // 对于特殊的 trait 可以使用 derive 实现
            struct T;
            impl Derived for T {}
            impl Base for T {}
            let x = T;
            println!("{:?}", x);
        }
    }
    // trait 别名
    {
        // 已经确定类型特定使用场景的情况下，可以为它设定一个别名
    }
    // 标准库中常见的 trait 简介
    {
        {
            // Display 和 Debug
            use std::fmt::Formatter;
            use std::fmt::Error;
            pub trait MyDisplay {
                fn fmt(&self, f: &mut Formatter) -> Result<(), Error>;
            }

            pub trait MyDebug {
                fn fmt(&self, f: &mut Formatter) -> Result<(), Error>;
            }

            use std::fmt::Display;

            #[derive(Debug)]
            struct T {
                field1: i32,
                field2: i32,
            }

            impl Display for T {
                fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
                    write!(f, "{{field1:{},field2:{}}}", self.field1, self.field2)
                }
            }

            let var = T {field1:1,field2:1};
            println!("{}",var);
            println!("{:?}",var);
            println!("{:#?}",var);
        }
        {
            // partialOrd, Ord, PartialEq, Eq
            let nan = std::f32::NAN;
            let x = 1.0f32;
            println!("{}", nan < x);
            println!("{}", nan > x);
            println!("{}", nan == x);
            use std::cmp::Ordering;
            pub trait MyPartialOrd<Rhs: ?Sized = Self>: PartialEq<Rhs> {
                fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;
                fn lt(&self,other:&Rhs) -> bool;
                fn rt(&self,other:&Rhs) -> bool;
                fn le(&self,other:&Rhs) -> bool;
                fn re(&self,other:&Rhs) -> bool;
            }
            pub trait Ord: Eq + MyPartialOrd<Self> {
                fn cmp(&self,other:&Self) -> Ordering;
            }
            // 无法对浮点数类型求出最大值
        }
        // sized 是定长类型
        // dst 是不定长类型
        {
            // default
            // rust 坚决反对 ad hoc 方式的函数重载
            // 对于默认构造函数，rust 中提供了 default trait
            trait MyDefault {
                fn default() -> Self;
            }
        }
    }
}