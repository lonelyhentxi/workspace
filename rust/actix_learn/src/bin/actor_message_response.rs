use actix::dev::{MessageResponse,ResponseChannel};
use actix::prelude::*;
use futures::Future;

enum Messages {
    Ping,
    Pong,
}

enum Responses {
    GotPing,
    GotPong,
}

impl<A,M> MessageResponse<A,M> for Responses where A:Actor,M:Message<Result=Responses> {
    fn handle<R: ResponseChannel<M>>(self, _: &mut A::Context, tx: Option<R>) {
        if let Some(tx) = tx {
            tx.send(self)
        }
    }
}

impl Message for Messages {
    type Result = Responses;
}

struct MyActor;

impl Actor for MyActor {
    type Context = Context<Self>;
    fn started(&mut self, _: &mut Context<Self>) {
        println!("Actor is alive");
    }
    fn stopped(&mut self, _: &mut Context<Self>) {
        println!("Actor is stopped");
    }
}

impl Handler<Messages> for MyActor {
    type Result = Responses;
    fn handle(&mut self,msg: Messages, _: &mut Context<Self>) -> Self::Result {
        match msg {
            Messages::Ping => Responses::GotPing,
            Messages::Pong => Responses::GotPong,
        }
    }
}

fn main() {
    let sys = System::new("example");
    let addr = MyActor.start();
    let ping_future = addr.send(Messages::Ping);
    let pong_future = addr.send(Messages::Pong);
    Arbiter::spawn(pong_future
    .map(|res| {
        match res {
            Responses::GotPing => println!("Ping received"),
            Responses::GotPong => println!("Pong received"),
        }
    })
    .map_err(|e| {
        println!("Actor is probably died: {}", e);
    }));
    Arbiter::spawn(ping_future
    .map(|res| {
        match res {
            Responses::GotPing => println!("Ping received"),
            Responses::GotPong => println!("Pong received"),
        }
    })
    .map_err(|e| {
        println!("Actor is probably died: {}", e);
    }));
    sys.run();
}