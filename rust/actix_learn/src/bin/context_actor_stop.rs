extern crate actix;
extern crate futures;
use futures::Future;
use actix::prelude::*;
struct MyActor {
   count: usize,
}
impl Actor for MyActor {
    type Context = Context<Self>;
}

struct Ping(usize);

impl Message for Ping {
   type Result = usize;
}
impl Handler<Ping> for MyActor {
    type Result = usize;

    fn handle(&mut self, msg: Ping, ctx: &mut Context<Self>) -> Self::Result {
        self.count += msg.0;

        if self.count > 5 {
            println!("Shutting down ping receiver.");
            ctx.stop()
        }

        self.count
    }
}

fn main() {
    let system = System::new("test");
    let addr = MyActor{count: 10}.start();
    let addr_2 = addr.clone();
    let res = addr.send(Ping(6));

    Arbiter::spawn(
    res.map(move |_res| {
        assert!(addr_2.try_send(Ping(6)).is_err());
        System::current().stop();
    })
    .map_err(|_| ()));
    system.run();
}