extern crate actix;
extern crate futures;

use std::io;
use actix::prelude::*;
use futures::Future;

struct Ping;
impl Message for Ping {
    type Result = Result<bool,io::Error>;
}

struct MyActor;
impl Actor for MyActor {
    type Context = Context<Self>;
    fn started(&mut self, _ctx: &mut Context<Self>) {
       println!("Actor is alive");
    }
    fn stopped(&mut self, _ctx: &mut Context<Self>) {
       println!("Actor is stopped");
    }
}

impl Handler<Ping> for MyActor {
    type Result = Result<bool, io::Error>;
    fn handle(&mut self,_msg: Ping, _ctx: &mut Context<Self>) -> Self::Result {
        println!("Ping received");
        Ok(true)
    }
}

fn main() {
    let sys = System::new("example");
    let addr = MyActor.start();
    let result = addr.send(Ping);
    Arbiter::spawn(
        result.map(|res| {
            match res {
                Ok(result) => println!("Got result: {}", result),
                Err(err) => println!("Got error: {}", err),
            }
        })
        .map_err(|e| {
            println!("Actor is probably died: {}", e);
        }));
    sys.run();
}