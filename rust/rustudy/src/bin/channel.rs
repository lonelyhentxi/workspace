use std::sync::mpsc::{Sender,Receiver};
use std::sync::mpsc;
use std::thread;

static NTHREADS: i32 = 3;

fn main() {
    let (tx,rx):(Sender<i32>,Receiver<i32>) = mpsc::channel();
    for id in 0..NTHREADS {
        let thread_tx = tx.clone();
        thread::spawn(move||{
            thread_tx.send(id).unwrap();
            println!("thread {} finished",id);
        });
    }
    let mut ids = Vec::with_capacity(NTHREADS as usize);
    for _ in 0..NTHREADS {
        ids.push(rx.recv());
    }
    println!("{:?}",ids);
}