fn main() {
    // 线程安全
    {
        // 启动线程
        {
            // 通常的使用方法
            use std::io;
            use std::io::prelude::*;
            use std::thread;

            // JoinHandle<ClosureType>
            let child = thread::spawn(move || {
                // 这里是新建线程的执行逻辑
                let stdout = io::stdout();
                let handle = stdout.lock();
                let mut buf = io::BufWriter::new(handle);
                buf.write("new thread created.\n".as_bytes()).unwrap();
            });
            let _ret = child.join();
        }
        {
            // 如果要为子线程指定更多的参数信息
            use std::thread;

            thread::Builder::new()
                .name("child1".to_string())
                .spawn(move || {
                    println!("Hello, world!");
                })
                .unwrap()
                .join()
                .unwrap();
        }
        // thread 模块还提供了
        // thread::sleep(dur:Duration) // 是当前线程等待一段时间继续执行。
        // 等待的时间内，线程调度器会调度其他的线程来执行。
        // thread::yield_now() // 放弃当前线程的执行，要求线程调度器执行线程切换
        // thread::current() // 获得当前的线程
        // thread::park() // 暂停当前的线程，进入等待状态，当 thread::Thread::unpark 方法被调用的时候，可以恢复运行
        // thread::Thread::unpark(&self) // 恢复一个线程的执行 
        {
            use std::thread;
            use std::time::Duration;

            let t = thread::Builder::new()
                .name(String::from("child1"))
                .spawn(move || {
                    println!("enter child thread.");
                    thread::park();
                    println!("resume child thread.");
                }).unwrap();
                thread::sleep(Duration::new(1,0));
                t.thread().unpark();
                t.join().unwrap();
                println!("child thread finished.");
        }
    }
    {
        // 免数据竞争
        {
            // 失败，不能肯定 health 的生命周期
            /*
            use std::thread;
            let mut health = 12;
            thread::spawn(|| {
                health *= 2;
            });
            */
        }
        {
            // 失败，由于是复制，值没有改变
            use std::thread;
            let mut health = 12;
            thread::spawn(move || {
                health *= 2;
            });
            println!("{}",health);
        }
        {
            use std::thread;
            let mut v: Vec<i32> = vec![];
            thread::spawn(move || {
                v.push(1);
            });
            // 失败，v 已经被移动到线程内，外部不能再使用
            // println!("{:?}",v);
        }
        // 我们没有办法在多线程中直接读写普通的共享变量
        // 除非使用 rust 提供的线程安全相关的设施
        {
            // 数据竞争相关问题：
            // 1. 数据共享：有多个线程同时访问一份数据
            // 2. 数据修改：至少存在一个线程对数据做修改
            // 3. 没有同步：至少存在一个线程对数据的访问没有使用同步措施
            // 过去或者当下常见的解决方式：
            // 1. 禁止数据共享，比如 actor-based concurrency。
            //    多线程之间的通信仅靠发送消息来实现，而不是通过共享数据来实现。
            // 2. 可以禁止数据修改，比如 functional programming，许多函数式
            //    编程语言限制了数据的可变性，而对共享性没有限制。
        }
        {
            // rust 的设计思路
            // rust 允许存在可变变量，允许状态共享，同时也做到了完整无遗漏的线程安全检查。
            // rust 既可以支持多线程数据共享的风格，也可以支持消息通信的风格
        }
    }
    {
        // Send & Sync
        // rust 免疫数据竞争的功臣是 std::marker 中的 Send，Sync
        // 如果类型实现了 Sync 类型，那说明在不同的线程中使用同一个 &T 是安全的
        // 如果类型实现了 Send 类型，那说明这个类型的变量在不同的线程中传递所有权是安全的
        {
            /*
            pub fn spawn<F,T>(f:F) -> JoinHandle<T>
                where F:FnOnce() -> T,F:Send + 'static,T:Send + 'static;
            */
        }
    }
}
