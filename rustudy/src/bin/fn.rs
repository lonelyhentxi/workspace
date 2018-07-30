// fn

fn is_divisible_by(lhs: u32, rhs: u32) -> bool {
    if rhs == 0 {
        return false;
    }
    lhs % rhs == 0
}

fn fizzbuzz(n: u32) {
    if is_divisible_by(n, 15) {
        println!("fizzbuzz");
    } else if is_divisible_by(n, 3) {
        println!("fizz");
    } else if is_divisible_by(n, 5) {
        println!("buzz");
    } else {
        println!("{}", n);
    }
}

fn fizzbuzz_to(n: u32) {
    for n in 1..n + 1 {
        fizzbuzz(n);
    }
}

// methods

struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn origin() -> Point {
        Point { x: 0.0, y: 0.0 }
    }
    fn new(x: f64, y: f64) -> Point {
        Point { x, y }
    }
}

struct Rectangle {
    p1: Point,
    p2: Point,
}

impl Rectangle {
    fn area(&self) -> f64 {
        let Point { x: x1, y: y1 } = self.p1;
        let Point { x: x2, y: y2 } = self.p2;
        ((x1 - x2) * (y1 - y2)).abs()
    }
    fn perimeter(&self) -> f64 {
        let Point { x: x1, y: y1 } = self.p1;
        let Point { x: x2, y: y2 } = self.p2;

        2.0 * ((x1 - x2).abs() + (y1 - y2).abs())
    }
    fn translate(&mut self, x: f64, y: f64) {
        self.p1.x += x;
        self.p2.x += x;
        self.p2.y += y;
        self.p1.y += y;
    }
}

struct Pair(Box<i32>, Box<i32>);

impl Pair {
    fn destroy(self) {
        let Pair(first, second) = self;
        println!("Destroying Pair({}, {})", first, second);
    }
}

fn main() {
    fizzbuzz_to(100);
    let rectangle = Rectangle {
        p1: Point::origin(),
        p2: Point::new(1.0, 1.0),
    };
    println!("Rectangle perimeter: {}", rectangle.perimeter());
    println!("Rectangle area: {}", rectangle.area());
    let mut square = Rectangle {
        p1: Point::origin(),
        p2: Point::new(1.0, 1.0),
    };
    square.translate(1.0, 1.0);
    let pair = Pair(Box::new(1), Box::new(2));
    pair.destroy();
    // closures
    fn function(i: i32) -> i32 { i + 1 };
    let closure_annotated = |i: i32| -> i32 { i + 1 };
    let closure_inferred = |i| i + 1;
    let i = 1;
    println!("function: {}", function(i));
    println!("closure_annotated: {}", closure_annotated(i));
    println!("closure_inferred: {}", closure_inferred(i));
    let one = || 1;
    println!("closure returning one: {}", one());
    // capture
    use std::mem;
    let color = "green";
    let print = || println!("`color`: {}", color);
    print();
    print();
    let mut count = 0;
    let mut inc = || {
        count += 1;
        println!("`color`: {}", count);
    };
    inc();
    inc();
    let movable = Box::new(3);
    let consume = || {
        println!("`movable`: {:?}", movable);
        mem::drop(movable);
    };
    consume();
    // input parameters
    fn apply<F>(f: F) where F: FnOnce() {
        f();
    }
    fn apply_to_3<F>(f: F) -> i32 where F: Fn(i32) -> i32 {
        f(3)
    }
    let greeting = "hello";
    let mut farewell = "goodbye".to_owned();
    let diary = || {
        println!("I said {}.", greeting);
        farewell.push_str("!!!");
        println!("Then I screamed {}.", farewell);
        println!("Now I can sleep. zzzzz");
        // TODO: has been convert to fnOnce ?
        mem::drop(farewell);
    };
    apply(diary);
    let double = |x| 2 * x;
    println!("3 doubled: {}", apply_to_3(double));
    // input function
    fn call_me<F: Fn()>(f: F) {
        f()
    }
    fn function1() {
        println!("I am a function");
    }
    let closure = || println!("I am a closure!");
    call_me(closure);
    call_me(function1);
    // output parameters
    fn create_fn() -> Box<Fn()> {
        let text = "Fn".to_owned();
        Box::new(move || println!("This is a: {}", text))
    }

    fn create_fnmut() -> Box<FnMut()> {
        let text = "FnMut".to_owned();
        Box::new(move || println!("This is a: {}", text))
    }

    let fn_plain = create_fn();
    let mut fn_mut = create_fnmut();
    fn_plain();
    fn_mut();
    // std example
    let vec1 = vec![1, 2, 3];
    let vec2 = vec![4, 5, 6];
    println!("2 in vec1: {}", vec1.iter().any(|&x| x == 2));
    println!("2 in vec2: {}", vec2.into_iter().any(|x| x == 2));
    let array1 = [1, 2, 3];
    let array2 = [4, 5, 6];
    println!("2 in array1: {}", array1.iter().any(|&x| x == 2));
    // TODO: lose code intelligence
    println!("2 in array2: {}", array2.into_iter().any(|&x|x==2));
    let vec1 = vec![1, 2, 3];
    let vec2 = vec![4, 5, 6];
    let array1 = [1, 2, 3];
    let array2 = [4, 5, 6];
    println!("Find 2 in vec1: {:?}",vec1.iter().find(|&&x| x==2));
    println!("Find 2 in vec2: {:?}",vec2.into_iter().find(|&x|x==2));
    println!("Find 2 in array1: {:?}", array1.iter()     .find(|&&x| x == 2));
    println!("Find 2 in array2: {:?}", array2.into_iter().find(|&&x| x == 2));
    // hof
    fn is_odd(n: u32) -> bool {
        n%2==1
    }
    println!("Find the sum of all the squared odd numbers under 1000");
    let upper = 1000;
    let mut acc = 0;
    for n in 0.. {
        let n_squared = n*n;
        if n_squared >= upper {
            break;
        } else if is_odd(n_squared) {
            acc += n_squared;
        }
    }
    println!("imperative style: {}",acc);
    let sum_of_squared_odd_numbers: u32 = (0..).map(|n| n*n)
        .take_while(|&n| n<upper)
        .filter(|&n| is_odd(n))
        .fold(0, |sum,i| sum+i);
    println!("functional style: {}",sum_of_squared_odd_numbers);
}