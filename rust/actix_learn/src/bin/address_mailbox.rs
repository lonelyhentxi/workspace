#[macro_use]
extern crate actix;
use actix::prelude::*;
#[derive(Message)]
struct Signal(usize);

#[derive(Message)]
struct Subscribe(pub Recipient<Signal>);

struct ProcessSignals {
    subscribers: Vec<Recipient<Signal>>,
}

impl Actor for ProcessSignals {
    type Context = Context<Self>;
}

impl ProcessSignals {
    fn send_signal(&mut self,sig: usize) {
        for subscr in &self.subscribers {
            subscr.do_send(Signal(sig));
        }
    }
}

impl Handler<Subscribe> for ProcessSignals {
    type Result = ();
    fn handle(&mut self,msg: Subscribe,_:&mut Self::Context) {
        self.subscribers.push(msg.0);
    }
}

fn main() {
}
