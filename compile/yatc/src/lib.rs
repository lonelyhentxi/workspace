#![feature(box_syntax)]
#![feature(plugin)]
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate serde_derive;
extern crate libc;
extern crate llvm_sys;
extern crate llvm_sys_wrapper;
extern crate regex;
extern crate serde;

pub mod codegen;
pub mod cstring_manager;
pub mod driver;
pub mod lexer;
pub mod parser;
pub mod util;