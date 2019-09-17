use serde::Serialize;
use serde_json;
use std::process::exit;
use std::io::Write;
use std::fs::OpenOptions;

pub fn write_json_to_file<T:Serialize>(filename: &str,content: &T,err_prompt: &str) {
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(filename)
      .unwrap_or_else(|err| { writeln!(std::io::stderr(), "{},{}",err, err_prompt).ok(); exit(-1);});
    let output = serde_json::to_string(content)
      .unwrap_or_else(|err| { writeln!(std::io::stderr(), "{},{}",err, err_prompt).ok(); exit(-1);});
    file
      .write_all(&output.into_bytes())
      .unwrap_or_else(|err| { writeln!(std::io::stderr(), "{},{}",err, err_prompt).ok(); exit(-1);});
}