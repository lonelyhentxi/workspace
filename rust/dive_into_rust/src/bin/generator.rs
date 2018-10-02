
#![feature(generators,generator_trait)]
#![allow(dead_code)]
#![allow(unreachable_code)]
fn main() {
    // 生成器
    {
        // 简介
        // 当闭包中有 yield 关键字的时候，它就不是闭包，而是生成器
        // rust 中实现的是无栈生成器
        {
            use std::ops::{Generator, GeneratorState};

            let mut g = || {
                let mut curr: u64 = 1;
                let mut next: u64 = 1;
                loop {
                    let new_next = curr.checked_add(next);

                    if let Some(new_next) = new_next {
                        curr = next;
                        next = new_next;
                        yield curr; // til 18/10/02，intellj-rust still did not support the 'yield' syntax
                    } else {
                        return;
                    }
                }
            };

            loop {
                unsafe {
                    match g.resume() {
                        GeneratorState::Yielded(v) => println!("{}", v),
                        GeneratorState::Complete(_) => return
                    }
                }
            }
        }
    }
    {
        // 对比迭代器
        // 任何生成器，总能找到某种办法改写为功能相同的迭代器
    }
    {
        // 对比立即求值
        // 惰性求值的好处有用时才生产，可以无限等
    }
    {
        // 生成器的原理
        {
            // 生成器的原理简介
            // 生成器实际上是迭代器和立即求值的杂交
            // 当前的 resume 还不能传递参数，是不稳定版本，有 unsafe 标识，这个匿名类型实现了 Generator 这个 trait
            /*
            trait generator {
                type Yield;
                type Return;
                unsafe fn resume(&mut self) -> GeneratorState<Self::Yield,Self::Return>;
            }*/
            // 生成器内部有额外的成员，是跨 yield 语句存在的局部变量
            // 生成器内部的语句会成为 resume 的方法体
            // yield 被替换为 return State::Yield(_);return 被替换为 return State::Complete(_)
            // yield 会在传出值的同时保存当前跨 yield 局部变量值供恢复后使用
        }
        {
            // 自引用类型
            /*
            let _g = || {
                let local = 1;
                let ptr = &local;
                yield local;
                yield *ptr;
            };
            */
            // 编译错误，在生成器中出现了一个对象引用另一个对象的情况
            // 一旦双双移动，指针未变，地址已经变化
            {
                // 当下的设想
                // 允许用户创建自引用生成器，因为在调用 resume 方法之前的移动都是没有问题的
                // 一旦 resume 被调用过，就不能再移动对象
                // 解决的措施
                // 引入新的指针类型 PinMut 指向一个 T 类型的指针，作用是当指针存在的时候，指向的对象不能移动
                // 允许更多智能指针类型作为 self 变量的类型
            }
        }
    }
    {
        // 协程简介
        {
            // 协程指的是一套非抢占式多任务机制，可以实现多任务并行，最大的特点是并非内核调度
            // rust 使用极少的关键字+核心库的方式实现，避免不同方案的异步不一致
            // 设计的核心是 async await 和 Future、Executor（具体实现交给第三方库）
            // Future 的参数是 PinMut
            // 当前，async 的实现已经完成，await 的实现还存在争议
            // 参考 C#，Javascript，kotlin
            {
                // async fn f1(arg: u8) -> u8;
                // eq
                // fn f1(arg:u8)->impl Future<Output = u8>;
            }
            // 从语法上说，await 一定只能在 async 中出现
        }
    }
}