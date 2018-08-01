#![allow(dead_code)]
use std::mem;

#[derive(Clone,Copy)]
struct Point {
    x: f64,
    y: f64
}

#[allow(dead_code)]
struct Rectangle {
    p1: Point,
    p2: Point,
}

fn origin() -> Point {
    Point {x:0.0,y:0.0}
}

fn boxed_origin() -> Box<Point> {
    Box::new(Point { x: 0.0, y: 0.0 })
}

// ?
mod checked {
    #[derive(Debug)]
    enum MathError {
        DivisionByZero,
        NegativeLogarithm,
        NegativeSquareRoot,
    }

    type MathResult = Result<f64, MathError>;
    fn div(x: f64, y: f64) -> MathResult {
        if y == 0.0 {
            Err(MathError::DivisionByZero)
        } else {
            Ok(x / y)
        }
    }

    fn sqrt(x: f64) -> MathResult {
        if x < 0.0 {
            Err(MathError::NegativeSquareRoot)
        } else {
            Ok(x.sqrt())
        }
    }

    fn ln(x: f64) -> MathResult {
        if x < 0.0 {
            Err(MathError::NegativeLogarithm)
        } else {
            Ok(x.ln())
        }
    }

    fn op_(x:f64,y:f64) -> MathResult {
        let ratio = div(x,y)?;
        let ln = ln(ratio)?;
        sqrt(ln)
    }

    pub fn op(x: f64, y: f64) {
        match op_(x, y) {
            Err(why) => println!("{}",(match why {
                MathError::NegativeLogarithm
                => "logarithm of negative number",
                MathError::DivisionByZero
                => "division by zero",
                MathError::NegativeSquareRoot
                => "square root of negative number",
            })),
            Ok(value) => println!("{}", value),
        }
    }
}

// alt key types

use std::collections::HashMap;

#[derive(PartialEq,Eq,Hash)]
struct Account<'a> {
    username: &'a str,
    password: &'a str,
}

struct AccountInfo<'a> {
    name: &'a str,
    email: & 'a str,
}

type Accounts<'a> = HashMap<Account<'a>,AccountInfo<'a>>;

fn try_logon<'a>(accounts: &Accounts<'a>,username:&'a str,password: & 'a str) {
    println!("Username: {}", username);
    println!("Password: {}", password);
    println!("Attempting logon...");

    let logon = Account {
        username: username,
        password: password,
    };

    match accounts.get(&logon) {
        Some(account_info) => {
            println!("Successful logon!");
            println!("Name: {}", account_info.name);
            println!("Email: {}", account_info.email);
        },
        _ => println!("Login failed!"),
    }
}

fn main() {
    let point: Point  = origin();
    let rectangle: Rectangle = Rectangle {
        p1: origin(),
        p2: Point { x: 3.0, y: 4.0 }
    };
    let boxed_rectangle: Box<Rectangle> = Box::new(Rectangle {
        p1: origin(),
        p2: origin()
    });
    let boxed_point: Box<Point> = Box::new(origin());
    let box_in_a_box: Box<Box<Point>> = Box::new(boxed_origin());
    println!("Point occupies {} bytes in the stack",
             mem::size_of_val(&point));
    println!("Rectangle occupies {} bytes in the stack",
             mem::size_of_val(&rectangle));
    println!("Boxed point occupies {} bytes in the stack",
             mem::size_of_val(&boxed_point));
    println!("Boxed rectangle occupies {} bytes in the stack",
             mem::size_of_val(&boxed_rectangle));
    println!("Boxed box occupies {} bytes in the stack",
             mem::size_of_val(&box_in_a_box));
    let unboxed_point: Point = *boxed_point;
    println!("Unboxed point occupies {} bytes in the stack",
             mem::size_of_val(&unboxed_point));
    // vector
    let collected_iterator: Vec<i32> = (0..10).collect();
    println!("Collected (0..10) into: {:?}",collected_iterator);
    let mut xs = vec![1i32, 2, 3];
    println!("Initial vector: {:?}", xs);
    println!("Push 4 into the vector");
    xs.push(4);
    println!("Vector: {:?}", xs);
    println!("Vector size: {}", xs.len());
    println!("Second element: {}", xs[1]);
    println!("Pop last element: {:?}", xs.pop());
    // str
    let pangram: &'static str = "the quick brown fox jumps over the lazy dog";
    println!("Pangram: {}", pangram);
    println!("Words in reverse");
    for word in pangram.split_whitespace().rev() {
        println!("> {}", word);
    }
    let mut chars: Vec<char> = pangram.chars().collect();
    chars.sort();
    chars.dedup();
    let mut string = String::new();
    for c in chars {
        string.push(c);
        string.push_str(", ");
    }
    let chars_to_trim: &[char] = &[' ', ','];
    let trimmed_str: &str = string.trim_matches(chars_to_trim);
    println!("Used characters: {}", trimmed_str);
    let alice = String::from("I like dogs");
    let bob: String = alice.replace("dog", "cat");
    println!("Alice says: {}", alice);
    println!("Bob says: {}", bob);
    // ?
    checked::op(1.0,10.0);
    // panic ...
    // hash map
    use std::collections::HashMap;
    fn call(number:&str) -> &str {
        match number {
            "798-1364" => "We're sorry, the call cannot be completed as dialed.
            Please hang up and try again.",
            "645-7689" => "Hello, this is Mr. Awesome's Pizza. My name is Fred.
            What can I get for you today?",
            _ => "Hi! Who is this again?"
        }
    }
    let mut contacts = HashMap::new();
    contacts.insert("Daniel", "798-1364");
    contacts.insert("Ashley", "645-7689");
    contacts.insert("Katie", "435-8291");
    contacts.insert("Robert", "956-1745");
    match contacts.get(&"Daniel") {
        Some(&number) => println!("Calling Daniel: {}", call(number)),
        _ => println!("Don't have Daniel's number."),
    }
    contacts.insert("Daniel", "164-6743");

    match contacts.get(&"Ashley") {
        Some(&number) => println!("Calling Ashley: {}", call(number)),
        _ => println!("Don't have Ashley's number."),
    }

    contacts.remove(&("Ashley"));
    for (contact, &number) in contacts.iter() {
        println!("Calling {}: {}",contact, call(number));
    }
    // alt key types
    let mut accounts: Accounts = HashMap::new();
    let account = Account {
        username: "j.everyman",
        password: "password123",
    };

    let account_info = AccountInfo {
        name: "John Everyman",
        email: "j.everyman@email.com",
    };

    accounts.insert(account,account_info);
    try_logon(&accounts, "j.everyman", "psasword123");

    try_logon(&accounts, "j.everyman", "password123");
    // hash set
    use std::collections::HashSet;
    let mut a: HashSet<i32> = vec!(1i32,2,3).into_iter().collect();
    let mut b: HashSet<i32> = vec!(2i32, 3, 4).into_iter().collect();
    assert!(a.insert(4));
    assert!(a.contains(&4));
    // assert!(b.insert(4), "Value 4 is already in set B!");
    b.insert(5);
    println!("A: {:?}", a);
    println!("B: {:?}", b);
    println!("Union: {:?}", a.union(&b).collect::<Vec<&i32>>());
    println!("Difference: {:?}", a.difference(&b).collect::<Vec<&i32>>());
    println!("Intersection: {:?}", a.intersection(&b).collect::<Vec<&i32>>());
    println!("Symmetric Difference: {:?}",
             a.symmetric_difference(&b).collect::<Vec<&i32>>());
}