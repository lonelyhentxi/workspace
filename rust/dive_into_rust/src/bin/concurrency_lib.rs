extern crate threadpool;
extern crate scoped_threadpool;
extern crate parking_lot;
extern crate crossbeam;
extern crate rayon;
#[macro_use]
extern crate crossbeam_channel;
fn main() {
    // 第三方并行开发库
    {
        // threadpool
        use threadpool::ThreadPool;
        use std::sync::mpsc::channel;

        let n_workers = 4;
        let n_jobs = 8;
        let pool = ThreadPool::new(n_workers);

        let (tx,rx) = channel();
        for _ in 0..n_jobs {
            let tx = tx.clone();
            pool.execute(move || {
                tx.send(1).expect("channel will be there  waiting for the pool");
            })
        }
        assert_eq!(rx.iter().take(n_jobs).fold(0, |a,b| a+b),8);
        println!("assert eq succeed");
    }
    {
        // scoped-threadpool
        {
            use scoped_threadpool::Pool;

            let mut pool = Pool::new(4);
            let mut vec = vec![0,2,3,4,5,6,7];
            pool.scoped(|scope| {
                for e in &mut vec {
                    scope.execute(move || {
                        *e += 1;
                    })
                }
            });
        }
        {
            /*
            pub fn spawn<F,T>(f:F) -> JoinHandle<T> where F: FnOnce() -> T, F: Send + 'static, T:Send + 'static;
            fn scoped<'pool,'scoped,F,R>(&'pool mut self,f:F) -> R where F: FnOnce(&Scope<'pool,'scope>) -> R;
            */
            // 分别的实现
        }
    }
    {
        // parking_lot
        // 性能有极高要求的操作系统同步原语
        {
            use std::sync::Arc;
            use parking_lot::Mutex;
            use std::thread;
            use std::sync::mpsc::channel;

            const N: usize = 10;
            let data = Arc::new(Mutex::new(0));
            let (tx,rx) = channel();
            for _ in 0..10 {
                let (data,tx) = (data.clone(),tx.clone());
                thread::spawn(move || {
                    let mut data = data.lock();
                    *data += 1;
                    if *data == N {
                        tx.send(()).unwrap();
                    }
                });
            }
            println!("{:?}", rx.recv().unwrap());
        }
    }
    {
        // crossbeam
        // 包含了很多方面的功能
        use crossbeam_channel as channel;

        fn seek<'a>(name: &'a str, tx: &channel::Sender<&'a str>,rx:&channel::Receiver<&'a str>) {
            select! {
                recv(rx,peer) => println!("{} received a message from {}",name, peer.unwrap()),
                send(tx,name) => {},
            }
        }

        let people = vec!["Anna","Bob","Cody","Dave","Eva"];
        let (tx,rx) = channel::bounded(1);
        let (tx,rx) = (&tx,&rx);
        crossbeam::scope(|s| {
            for name in people {
                s.spawn(move || seek(name,tx,rx));
            }
        });
        if let Some(name) = rx.try_recv() {
            println!("No one received {}'s message.",name );
        }
    }
    {
        // rayon
        {
            // rayon 的 API 主要有两种：
            // 并行迭代器 —— 对一个可迭代的序列调用 par_iter 方法，就可以产生一个并行迭代器
            // join 函数 —— 它可以把一个递归的分治算法变成并行执行
            {
                use rayon::prelude::*;
                fn _sum_of_squares(input: &[i32]) -> i32 {
                    input.par_iter()
                        .map(|&i| i*i)
                        .sum()
                }
            }
            // 也有 mut 版本
            {
                use rayon::prelude::*;
                fn _increment_all(input: &mut [i32]) {
                    input.par_iter_mut()
                        .for_each(|p| *p+=1);
                }
            }
            // Rayon 的另外一种使用方式是调用 join 函数，这个函数特别适合于分治算法。
            {
                fn partition<T:PartialOrd+Send>(v: &mut [T]) -> usize {
                    let pivot = v.len() - 1;
                    let mut i = 0;
                    for j in 0..pivot {
                        if v[j] < v[pivot] {
                            v.swap(i,j);
                            i+=1;
                        }
                    }
                    v.swap(i,pivot);
                    i
                }

                fn quick_sort<T:PartialOrd+Send>(v: &mut[T]) {
                    if v.len() <=1 {
                        return;
                    }
                    let mid = partition(v);
                    let (lo,hi) = v.split_at_mut(mid);
                    rayon::join(|| quick_sort(lo), || quick_sort(hi));
                }

                let mut v = vec![10,9,8,7,6,5,4,3,2,1];
                quick_sort(&mut v);
                println!("{:?}",v);
            }
        }
    }
}