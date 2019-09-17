#![allow(dead_code)]
fn main() {
    // 状态共享
    {
        // Arc
        // Arc 是 Rc 的线程安全版本
        {
            // Arc 使用实例
            use std::sync::Arc;
            use std::thread;

            let numbers: Vec<_> = (0..100u32).collect();
            let shared_numbers = Arc::new(numbers);
            for _ in 0..10 {
                let child_numbers = shared_numbers.clone();
                thread::spawn(move || {
                    let _local_numbers = &child_numbers[..];
                });
            }
            // 如果把上面的 Arc 改为 Rc，就会发生编译错误
            // 因为没有满足 Send 条件
        }
        {
            // 函数签名
            /*
            unsafe impl<T:?Sized + Sync + Send> Send for Arc<T> {};
            unsafe impl<T:?Sized + Sync + Send> Sync for Arc<T> {};
            impl<T:?Sized> !std::marker::Send for Rc<T> {};
            impl<T:?Sized> !std::marker::Sync for Rc<T> {};
            */
        }
    }
    {
        // Mutex
        // Arc 是只读性质的，如果要修改，就会导致编译错误
        // 一般需要线程安全版本的内部可变性
        // Cell RefCell 不具有内部可变性
        // 此时需要 RwLock 和 Mutex
        {
            use std::sync::Arc;
            use std::sync::Mutex;
            use std::thread;

            const COUNT: u32 = 1000000;

            let global = Arc::new(Mutex::new(0));
            let clone1 = global.clone();
            let thread1 = thread::spawn(move || {
                for _ in 0..COUNT {
                    let mut value = clone1.lock().unwrap();
                    *value += 1;
                }
            });
            let clone2 = global.clone();
            let thread2 = thread::spawn(move || {
                for _ in 0..COUNT {
                    let mut value = clone2.lock().unwrap();
                    *value -= 1;
                }
            });
            thread1.join().ok();
            thread2.join().ok();
            println!("final value: {:?}", global);
        }
        {
            /*
            unsafe impl<T:?Sized+Send> Send for Mutex<T> {};
            unsafe impl<T:?Sized+Sync> Sync for Mutex<T> {};
            pub fn lock(&self) -> LockResult<MutexGuard<T>>;
            type LockResult<Guard> = Result<Guard,PoisonError<Guard>>;
            */
            // 如果 Mutex 在锁住的时候发生了 panic，就会将 Mutex 置位有毒的状态
            // 有毒之后 lock() 都会失败
            // 如果有需要可以使用 PoisonError::into_inner() 获得内部数据
        }
    }
    {
        // RwLock
        // RwLock 同样调用 lock 方法，对内部的读写则是提供了 read write 来做
        {
            use std::sync::Arc;
            use std::sync::RwLock;
            use std::thread;

            const COUNT: u32 = 1_000_000;
            let global = Arc::new(RwLock::new(0));
            let clone1 = global.clone();
            let thread1 = thread::spawn(move || {
                for _ in 0..COUNT {
                    let mut value = clone1.write().unwrap();
                    *value += 1;
                }
            });
            let clone2 = global.clone();
            let thread2 = thread::spawn(move || {
                for _ in 0..COUNT {
                    let mut value = clone2.write().unwrap();
                    *value -= 1;
                }
            });
            thread1.join().ok();
            thread2.join().ok();
            println!("final value {:?}", global);
        }
    }
    {
        // Atomic
        // 转化为特殊的 CPU 指令
        {
            use std::sync::atomic::{AtomicIsize, Ordering};
            use std::sync::Arc;
            use std::thread;
            const COUNT: u32 = 1_000_000;
            let global = Arc::new(AtomicIsize::new(0));

            let clone1 = global.clone();
            let thread1 = thread::spawn(move || {
                for _ in 0..COUNT {
                    clone1.fetch_add(1, Ordering::SeqCst);
                }
            });

            let clone2 = global.clone();
            let thread2 = thread::spawn(move || {
                for _ in 0..COUNT {
                    clone2.fetch_sub(1, Ordering::SeqCst);
                }
            });

            thread1.join().ok();
            thread2.join().ok();
            println!("final value: {:?}", global);
        }
    }
    {
        // 死锁
        {
            use std::sync::{Arc, Mutex};
            use std::thread;
            use std::time::Duration;

            struct Philosopher {
                name: String,
                left: usize,
                right: usize,
            }

            struct Table {
                forks: Vec<Mutex<()>>,
            }

            impl Philosopher {
                fn new(name: &str, left: usize, right: usize) -> Philosopher {
                    Philosopher {
                        name: name.into(),
                        left,
                        right,
                    }
                }

                fn eat(&self, table: &Table) {
                    let _left = table.forks[self.left].lock().unwrap();
                    println!("{} take left fork.", self.name);
                    thread::sleep(Duration::from_secs(10));
                    let _right = table.forks[self.right].lock().unwrap();
                    println!("{} take right fork.", self.name);
                    // thread::sleep(Duration::from_secs(1)); // 原书在这里等待，可能由于 debug 模式性能低，左边还没有全部锁住就得到右边解锁了
                    println!("{} is done eating.", self.name);
                }
            }

            fn test() {
                let table = Arc::new(Table {
                    forks: vec![
                        Mutex::new(()),
                        Mutex::new(()),
                        Mutex::new(()),
                        Mutex::new(()),
                        Mutex::new(()),
                    ],
                });
                let philosophers = vec![
                    Philosopher::new("Judith Butler", 0, 1),
                    Philosopher::new("Gilles Deleuze", 1, 2),
                    Philosopher::new("Karl Marx", 2, 3),
                    Philosopher::new("Emma Goldman", 3, 4),
                    Philosopher::new("Michel Foucault", 4, 0),
                ];
                let handles: Vec<_> = philosophers
                    .into_iter()
                    .map(|p| {
                        let table = table.clone();
                        thread::spawn(move || {
                            p.eat(&table);
                        })
                    })
                    .collect();
                for h in handles {
                    h.join().unwrap();
                }
            }
        }
    }
    {
        // Barrier
        // Barrier 是这样一个类型，使用一个整数做初始化
        // 可以使得多个线程在某个点上一起等待，然后再继续执行
        {
            use std::sync::{Arc, Barrier};
            use std::thread;

            let barrier = Arc::new(Barrier::new(10));
            let mut handles = vec![];
            for _ in 0..10 {
                let c = barrier.clone();
                let t = thread::spawn(move || {
                    println!("before wait");
                    c.wait();
                    println!("after wait");
                });
                handles.push(t);
            }
            for h in handles {
                h.join().ok();
            }
        }
    }
    {
        // Condvar
        // Condvar 是条件变量，可以用于等待某个事件的发生
        // 在等待的时候，这个线程处于阻塞状态，并不消耗 CPU 资源
        // 一个 Condvar 应当对应一个 Mutex，否则会 panic
        {
            use std::sync::{Arc, Condvar, Mutex};
            use std::thread;
            use std::time::Duration;

            let pair = Arc::new((Mutex::new(false), Condvar::new()));
            let pair2 = pair.clone();
            let thread1 = thread::spawn(move || {
                thread::sleep(Duration::from_secs(1));
                let &(ref lock, ref cvar) = &*pair2;
                let mut started = lock.lock().unwrap();
                *started = true;
                cvar.notify_one();
                println!("child thread {}", *started);
            });

            let &(ref lock, ref cvar) = &*pair;
            let mut started = lock.lock().unwrap();
            println!("before wait {}", *started);
            while !*started {
                started = cvar.wait(started).unwrap();
            }
            println!("after wait {}", *started);
            thread1.join().ok();
        }
    }
    {
        // 全局变量
        {
            // 使用、修改可变全局变量
            static mut G: i32 = 1;
            unsafe {
                G = 2;
                println!("{}", G);
            }
            // 因为 rust 里有变量共享满足 Sync 的要求
            // 全局变量也是如此
        }
    }
    {
        // 线程局部存储
        // 线程局部变量的意思是，声明的这个变量看起来就像一个变量
        // 但是实际上在每一个线程分别有一个独立的存储地址
        {
            use std::cell::RefCell;
            use std::thread;
            thread_local!{
                static FOO: RefCell<i32> = RefCell::new(1);
            }

            FOO.with(|f|{
                println!("main thread value1 {:?}",*f.borrow());
                *f.borrow_mut() = 2;
                println!("main thread value2 {:?}",*f.borrow());
                let t = thread::spawn(move || {
                    FOO.with(|f| {
                        println!("child thread value1 {:?}",*f.borrow());
                        *f.borrow_mut() = 3;
                        println!("child thread value2 {:?}",*f.borrow());
                    })
                });
                t.join().ok();

                FOO.with(|f| {
                    println!("main thread value3 {:?}",*f.borrow());
                })
            })
        }
    }
    {
        // 总结
        // 设计一个可以应用于所有类型的通用 Atomic<T> 类型，这个常识已经由开源库 atomic-rs 实现了
    }
}
