extern crate actix;
extern crate futures;

use actix::prelude::*;
use futures::Future;

struct MyActor {
    count: usize,
}

struct Ping(usize);

impl Message for Ping {
    type Result = usize;
}

impl Actor for MyActor {
    type Context = Context<Self>;
}

impl Handler<Ping> for MyActor {
    type Result = usize;
    fn handle(&mut self, msg: Ping, _ctx:&mut Context<Self>) -> Self::Result {
        self.count += msg.0;
        self.count
    }
}

fn main() {
    let system = System::new("test");
    let addr = MyActor{count:10}.start();
    let res = addr.send(Ping(10));
    Arbiter::spawn(res.map(|res| {
        System::current().stop();
        println!("RESULT: {}", res == 20);
    }).map_err(|_| ()));
    system.run();
}