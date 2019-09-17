use std::os::raw::c_int;

#[link(name="rslib")]
extern "C" {
    fn add_square(a: c_int,b: c_int) -> c_int;
}

fn main() {
    let r = unsafe { add_square(2,2) };
    println!("{}",r);
}