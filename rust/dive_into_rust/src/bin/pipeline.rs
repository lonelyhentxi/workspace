fn main() {
    // 管道
    {
        // 除了在多线程共享变量
        // rust 标准库中提供了另一种线程之间的通信方式 mpsc
        // mpsc 代表的是多生产者单消费者先进先出队列
    }
    {
        // 异步管道
        // 发送和接收端之间存在一个缓冲区，不用管什么时候接受
        {
            use std::sync::mpsc::channel;
            use std::thread;
            let (tx, rx) = channel(); // 这样的设计保证了产生相同类型的泛型实现
            thread::spawn(move || {
                for i in 0..10 {
                    tx.send(i).unwrap();
                }
            });

            while let Ok(r) = rx.recv() {
                println!("received {}", r);
            }
        }
        {
            // 先进先出，但是线程顺序不一定
            // tx 可以 clone , rx 不可以 clone
            use std::sync::mpsc::channel;
            use std::thread;
            let (tx, rx) = channel();
            for i in 0..10 {
                let tx = tx.clone();
                thread::spawn(move || {
                    tx.send(i).unwrap();
                }).join().ok();
            };
            drop(tx); // 必须销毁，否则会持续等待

            while let Ok(r) = rx.recv() {
                println!("received {}", r);
            }
        }
    }
    {
        // 同步管道
        // 异步管道的缓冲区容量不限，直接返回
        // 同步管道必须要得到接收到的回复，才会返回
        {
            use std::thread;
            use std::sync::mpsc::sync_channel;

            let (tx,rx) = sync_channel(1);
            tx.send(1).unwrap();
            println!("send first");
            thread::spawn(move || {
                tx.send(2).unwrap();
                println!("send second");
            });

            println!("receive first {}", rx.recv().unwrap());
            println!("receive second {}", rx.recv().unwrap());
        }
    }
}
