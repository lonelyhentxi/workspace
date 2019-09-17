#![allow(unreachable_code)]
fn main() {
    {
        // 什么是 panic
        {
            fn _panic_intro() {
                let x: Option<i32> = None;
                x.unwrap();
            }
        }

    }
    {
        // panic 实现机制
        {
            // 在 rust 的实现机制有两种方式：unwind 和 abort
            // unwind 在发生 panic 的时候，会一层一层退出函数调用栈，在此过程中栈内的局部变量还能够局部析构
            // abort 方式在 panic 下会退出整个程序
            // unwind 在有些情况下不能很好的实现（尤其是嵌入式平台），编译器提供选项 panic=unwind
            fn _panic_impl() {
                use std::panic;
                panic::catch_unwind(|| {
                    let x: Option<i32> = None;
                    x.unwrap();
                    println!("interrupted.")
                }).ok();
                println!("continue to execute.");
            }
            _panic_impl();
        }
        {
            // 在 FFI 场景下使用，避免未处理好的代码传入 C 导致未定义的行为
            // 某些高级抽象机制需要阻止栈展开
        }
    }
    {
        // panic safety
        {
            // 异常安全存在四种层次的保证
            /*
                1. No-throw 这种层次的安全性保证了所有的异常都在内部正确地处理完毕，外部毫无影响。
                2. Strong exception safety 强安全保证保证异常发生的时候，可以回滚到初始状态，不会导致
                   状态不一致。
                3. Basic exception safety 基本异常保证保证不会导致异常泄漏。
                4. No exception safety 没有任何异常保证
            */
        }
        {
            use std::panic;
            use std::panic::AssertUnwindSafe;
            let mut x: Vec<i32> = vec![1];
            let mut y: Vec<i32> = vec![2];
            // catchunwind 要求必须满足 unwindsafe 保证
            panic::catch_unwind(AssertUnwindSafe(|| {
                x.push(10);
                panic!("user panic");
                y.push(100);
            })).ok();
            println!("assert unwind safe")
        }
        {
            use std::sync::Arc;
            use std::sync::Mutex;
            use std::thread;

            const COUNT: u32 = 1000000;

            let global = Arc::new(Mutex::new(0));
            let clone1 = global.clone();
            let thread1 = thread::spawn(move || {
               for _ in 0..COUNT {
                   match clone1.lock() {
                       Ok(mut value) => { *value+=1; },
                       Err(poisoned) => {
                           let mut value = poisoned.into_inner();
                           *value+=1;
                       }
                   }
               }
            });
            let clone2 = global.clone();
            let thread2 = thread::spawn(move || {
                for _ in 0..COUNT {
                    let mut value = clone2.lock().unwrap();
                    *value -= 1;
                    if *value < 100000 {
                        println!("make a panic");
                        panic!("");
                    }
                }
            });
            thread1.join().ok();
            thread2.join().ok();
            println!("final value: {:?}",global);
        }
    }
}