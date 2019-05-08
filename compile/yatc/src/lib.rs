#![feature(box_syntax)]
#![feature(plugin)]
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate serde_derive;
extern crate regex;
extern crate serde;

pub mod codegen;
pub mod lexer;
pub mod parser;
