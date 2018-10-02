#![feature(specialization)]
#![allow(dead_code)]
fn main() {
    // 泛型
    {
        // 数据结构中的泛型
        {
            enum MyOption<T> {
                Some(T),
                None
            }

            let _x: MyOption<i32> = MyOption::Some(64);
            let _y: MyOption<f64> = MyOption::None;
        }
        {
            struct S<T=i32> {
                data: T,
            }

            let v1 = S {data:0};
            let v2 = S::<bool> {data: false};
            println!("{} {}", v1.data, v2.data);
        }
        {
            // 编译器声明了一个泛型参数，但是没有被使用会报错
            // 想要实现仅仅做 identity 的泛型参数需要使用 PhantomData
        }
    }
    {
        // 函数中的泛型
        {
            fn compare_option<T,S>(first: Option<T>, second: Option<S>) -> bool {
                match(first,second) {
                    (Some(..),Some(..)) => true,
                    (None,None)=>true,
                    _ => false,
                }
            }
            println!("{}", compare_option(Some(1i32),Some(1.0f32)));
        }
        {
            // 一般情况下可以推导出类型参数，必要情况使用 fn_name::<type>(param)
            // rust 没有也不会有 C++ 的 ad hoc（专设）式函数重载
            // 泛型参数借助 trait 很大程度上实现了重载的功能
            let s = "hello";
            println!("{}", s.contains('a'));
            println!("{}", s.contains("abc"));
            println!("{}", s.contains(&['H'] as &[char]));
            println!("{}", s.contains(|c:char| c.len_utf8()>2));
            // fn contains<'a, P: pattern<'a>>(&'a self,pat:P) -> bool;
        }
    }
    {
        // impl 块中的泛型
        {
            /*
            impl<T,U> Into<U> for T
                where U:From<T>
            {
                fn into(self) -> U {
                    U::from(self)
                }
            }*/
            // 当希望为某一组类型统一实现一个 trait 的时候，可以使用 impl 泛型
        }
    }
    {
        // 泛型参数约束
        {
            // C++ 中执行泛型的类型检查是在实例化的时候做的
            // rust 中执行泛型的类型检查是立即进行的
            // C# 中的泛型类型检查也是立即进行的
        }
        {
            // 泛型约束有两种方法
            // 1. 在泛型参数声明的时候使用冒号指定
            // 2. 使用 where 子句指定
            // where 子句相较于冒号表达具有更强的表达能力，例如可以对成员做限制
            // 在有了泛型约束之后，编译器会在声明和实例化的地方都做检查
        }
    }
    {
        // 关联类型
        {
            // trait 中不仅可以包括方法、常量、还能够包含类型（类似C++）
            pub trait MyIterator {
                type Item;
            }
        }
        {
            // 关联类型也同样作为 trait 的泛型参数
            use std::iter::Iterator;
            use std::fmt::Debug;

            fn my_use_iter<ITEM,ITER>(mut iter:ITER) where ITER: Iterator<Item=ITEM>,ITEM: Debug {
                while let Some(i) = iter.next() {
                    println!("{:?}",i);
                }
            }

            let v = vec![1_i32,2,3];
            my_use_iter(v.iter());
        }
        {
            // 关联参数的优点
            {
                // 可读性可拓展性
                use std::iter::Iterator;
                use std::fmt::Debug;
                // 在有关联类型的情况下，可以将代码简化为
                fn _use_iter<ITER>(mut iter: ITER) where ITER: Iterator, ITER::Item: Debug {
                    while let Some(i) = iter.next() {
                        println!("{:?}", i);
                    }
                }
            }
            {
                // trait 的 impl 规则
                // 如果采用了关联类型的设计方案，就不能针对一个泛型类型做多份实现
                // 如果采用了trait泛型参数的方案，尽管麻烦，但是可以给定不同的类型参数，指定为不同泛型
            }
        }
    }
    {
        // 何时使用关联类型
        // 注：书本上这里是将关联类型比作输出类型参数，这个比喻并不恰当，只有用法上类似，但是意义上全然不同
        // 按上节结尾理解即可，略
    }
    {
        // 泛型特化
        // rust 的泛型特化还在开发中，目前的 nightly 1.30 的功能还比较基础
        // rust 不支持函数和结构体的特化，但是支持 impl 的特化
        {
            use std::fmt::Display;

            trait Example {
                fn call(&self);
            }

            impl<T> Example for T {
                default fn call(&self) {
                    println!("most generic");
                }
            }

            impl<T> Example for T where T:Display {
                default fn call(&self) {
                    println!("generic for Display, {}", self);
                }
            }

            impl Example for str {
                fn call(&self) {
                    println!("specialized for str {}", self);
                }
            }

            let v1 = vec![1i32,2,3];
            let v2 = 1_i32;
            let v3 = "hello";
            v1.call();
            v2.call();
            v3.call();
        }
        {
            // 和 C++ 中类型特化类似，越具体，越匹配
        }
        {
            // 特化的意义
            // 性能优化。 泛型特化可以为某些情况提供统一抽象下的特殊实现
            // 代码重用。泛型特化可以提供一些默认（但不完整）的实现，某些情况下减少重复代码。
            // 为 “高效继承”铺路。与 OOP 中的集成类似。
            {
                use std::fmt;

                pub trait MyToString {
                    fn to_string(&self) -> String;
                }

                impl <T:fmt::Display + ?Sized> MyToString for T {
                    #[inline]
                    default fn to_string(&self) -> String {
                        use std::fmt::Write;
                        let mut buf = String::new();
                        let _ = buf.write_fmt(format_args!("{}",self));
                        buf.shrink_to_fit();
                        buf
                    }
                }

                impl MyToString for str {
                    #[inline]
                    fn to_string(&self) -> String {
                        String::from(self)
                    }
                }

                let _x: String = "hello".into();
                let _x: String = String::from("hello");
                let _x: String = MyToString::to_string("hello");
            }
        }
        {
            // default 上下文关键字
            // default 不是全局关键字，只在这种场景下是特殊的
            // C++ 使用 virtual 关键字让一个方法可以在子类型中重写，现代 C++ 支持 override 和 final
            // C# 要求 virtual 定义虚函数，override 标记重写方法
            // Java 默认所有方法都是虚方法，支持使用 final 让方法不能重写
            // 如果 rust 中原有 trait 内部的方法不支持 defualt，特化会出错
        }
        {
            // 交叉 impl
            // 当为 B、C 都有限定泛型，但是 T:B+C 时，无法选择最佳实现
            // 当前标准中可以再实现它们的真子集
            // 但是这个解决方法并非万能，泛型特化的完整规则尚在酝酿，请关注提案的发展
        }
    }
}