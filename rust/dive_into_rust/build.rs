use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::panic;

fn main() {
    panic::catch_unwind(|| {

    }).unwrap_or_else(|_| {println!("There have a panic, but just a try")});
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path  = Path::new(&out_dir).join("commit_id.rs");
    let mut f = File::create(&dest_path).unwrap();

    let commit = Command::new("git")
        .arg("rev-parse")
        .arg("HEAD")
        .output()
        .expect("Failed to execute git command");
    let commit  = String::from_utf8(commit.stdout).expect("Invalid utf8 string");
    let output = format!(r#"pub const CURRENT_COMMIT_ID: &'static str = "{}";"#,commit);
    f.write_all(output.as_bytes()).unwrap();
}