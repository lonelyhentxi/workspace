#![crate_type = "cdylib"]

extern crate libc;

use libc::{c_int,c_char};
use std::ffi::CStr;

#[repr(C)]
#[no_mangle]
pub struct RustLogMessage {
    id: c_int,
    msg: *const c_char
}

#[no_mangle]
pub extern "C" fn rust_log(msg: RustLogMessage) {
    let s = unsafe { CStr::from_ptr(msg.msg) };
    println!("id:{} message:{:?}", msg.id, s);
}