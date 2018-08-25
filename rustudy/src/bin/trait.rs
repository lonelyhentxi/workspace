struct Sheep {
    naked: bool,
    name: &'static str,
}

trait Animal {
    fn new(name: &'static str) -> Self;
    fn name(&self) -> &'static str;
    fn noise(&self) -> &'static str;
    fn talk(&self) {
        println!("{} says {}",self.name(),self.noise());
    }
}

impl Animal for Sheep {
    fn new(name: &'static str) -> Sheep {
        Sheep {name,naked:false}
    }
    fn name(&self) -> &'static str {
        self.name
    }
    fn noise(&self) -> &'static str {
        if self.is_naked() {
            "baaaah?"
        } else {
            "baaaah!"
        }
    }
    fn talk(&self) {
        println!("{} pauses briefly... {}",self.name,self.noise());
    }
}

impl Sheep {
    fn is_naked(&self) -> bool {
        self.naked
    }
    fn shear(&mut self) {
        if self.is_naked() {
            println!("{} is already naked ...",self.name());
        } else {
            println!("{} gets a haircut!", self.name());
            self.naked = true;
        }
    }
}

#[derive(PartialEq,PartialOrd)]
struct Centimeters(f64);

#[derive(Debug)]
struct Inches(i32);

impl Inches {
    fn to_centimeters(&self) -> Centimeters {
        let &Inches(inches) = self;
        Centimeters(inches as f64*2.54)
    }
}

struct Seconds(i32);

use std::ops;
struct Foo;
struct Bar;
#[derive(Debug)]
struct FooBar;
#[derive(Debug)]
struct BarFoo;

impl ops::Add<Bar> for Foo {
    type Output = FooBar;
    fn add(self,_rhs: Bar) -> FooBar {
        println!("> Foo.add(Bar) was called");
        FooBar
    }
}

impl ops::Add<Foo> for Bar {
    type Output = BarFoo;

    fn add(self, _rhs: Foo) -> BarFoo {
        println!("> Bar.add(Foo) was called");

        BarFoo
    }
}

// drop

struct Droppable {
    name: &'static str,
}

impl Drop for Droppable {
    fn drop(&mut self){
        println!("> Dropping {}", self.name);
    }
}

// iter

struct Fibonacci {
    curr: u32,
    next: u32,
}

impl Iterator for Fibonacci {
    type Item = u32;
    fn next(&mut self) -> Option<u32> {
        let new_next = self.curr + self.next;
        self.curr = self.next;
        self.next = new_next;
        Some(self.curr)
    }
}

fn fibonacci() -> Fibonacci {
    Fibonacci {curr:1,next:1}
}

// clone
#[derive(Debug,Clone,Copy)]
struct Nil;

#[derive(Clone,Debug)]
struct Pair(Box<i32>,Box<i32>);

fn main() {
    // trait
    let mut dolly: Sheep = Animal::new("Dolly");
    dolly.talk();
    dolly.shear();
    dolly.talk();
    // derive
    let _one_second = Seconds(1);
    let foot = Inches(12);
    println!("One foot equals {:?}", foot);
    let meter = Centimeters(100.0);
    let cmp =
        if foot.to_centimeters() < meter {
            "smaller"
        } else {
            "bigger"
        };
    println!("One foot is {} than one meter.",cmp);
    // ops
    println!("Foo + Bar = {:?}", Foo + Bar);
    println!("Bar + Foo = {:?}", Bar + Foo);
    // drop
    let _a = Droppable { name: "a" };
    {
        let _b = Droppable { name: "b" };
        {
            let _c = Droppable { name: "c" };
            let _d = Droppable { name: "d" };
            println!("Exiting block B");
        }
        println!("Just exited block B");
        println!("Exiting block A");
    }
    println!("Just exited block A");
    drop(_a);
    println!("end of the main function");
    // iter
    let mut sequence = 0..3;
    println!("Four consecutive `next` calls on 0...3");
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());
    println!("Iterate through 0..3 using `for`");
    for i in 0..3 {
        println!("> {}", i);
    }
    println!("The first four terms of the Fibonacci sequence are: ");
    for i in fibonacci().take(4) {
        println!("> {}", i);
    }
    println!("The next four terms of the Fibonacci sequence are: ");
    for i in fibonacci().skip(4).take(4) {
        println!("> {}", i);
    }
    let array = [1u32, 3, 3, 7];
    println!("Iterate the following array {:?}", &array);
    for i in array.iter() {
        println!("> {}", i);
    }
    // clone
    let nil = Nil;
    let copied_nil = nil;
    println!("original: {:?}", nil);
    println!("copy: {:?}", copied_nil);
    let pair = Pair(Box::new(1), Box::new(2));
    println!("original: {:?}", pair);
    let moved_pair = pair;
    println!("copy: {:?}", moved_pair);
    let cloned_pair = moved_pair.clone();
    drop(moved_pair);
    println!("clone: {:?}", cloned_pair);
}