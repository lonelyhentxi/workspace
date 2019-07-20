extern crate actix;
use actix::prelude::*;

struct MyActor;
impl Actor for MyActor {
    type Context = Context<Self>;
    fn started(&mut self,ctx: &mut Context<Self>) {
        let _addr = ctx.address();
    }
}

fn main() {
    System::new("test");
    let _addr = MyActor.start();
}