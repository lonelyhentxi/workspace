use std::path::Path;

fn main() {
    let path = Path::new(".");
    let display = path.display();
    println!("Display of path is: {:?}",display);
    let new_path = path.join("a").join("b");
    match new_path.to_str() {
        None => panic!("new path is not a valid UTF-8 sequence"),
        Some(s) => println!("new path is {}", s),
    }
}