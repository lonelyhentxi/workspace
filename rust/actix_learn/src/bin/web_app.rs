extern crate actix_web;
use actix_web::{server, HttpResponse, App, http, HttpRequest};
use std::cell::Cell;

struct AppState {
    counter: Cell<usize>
}

fn index(req: HttpRequest<AppState>) -> String {
    let count = req.state().counter.get() + 1;
    req.state().counter.set(count);
    format!("Request number: {}", count)
}

fn main() {
    server::new(|| {
        /*let mut apps = vec![];
        apps.push(App::new()
            .prefix("/app1")
            .resource("/", |r| r.f(|r| HttpResponse::Ok())).boxed());
        apps.push(App::with_state(AppState { counter: Cell::new(0)})
            .resource("/", |r| r.f(index)).boxed());
        apps*/
        App::with_state(AppState { counter: Cell::new(0)})
                      .resource("/", |r| r.method(http::Method::GET).f(index)).finish()
    }).bind("127.0.0.1:8080").unwrap().run();
}