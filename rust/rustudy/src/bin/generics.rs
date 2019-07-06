struct A;

struct Single(A);

struct SingleGen<T>(T);

fn reg_fn(_s: Single) {}

fn gen_spec_t(_s: SingleGen<A>) {}

fn generic<T>(_s: SingleGen<T>) {}

#[allow(dead_code)]
struct GenericVal<T>(T);

impl GenericVal<f32> {}

impl GenericVal<A> {}

impl<T> GenericVal<T> {}

struct Val {
    val: f64
}

struct GenVal<T> {
    gen_val: T
}

impl Val {
    fn value(&self) -> &f64 { &self.val }
}

impl<T> GenVal<T> {
    fn value(&self) -> &T { &self.gen_val }
}

struct Empty;

struct Null;

trait DoubleDrop<T> {
    fn double_drop(self, _: T);
}

impl<T, U> DoubleDrop<T> for U {
    fn double_drop(self, _: T) {}
}

// bounds
use std::fmt::Debug;

trait HasArea {
    fn area(&self) -> f64;
}

impl HasArea for Rectangle {
    fn area(&self) -> f64 {
        self.length * self.height
    }
}

#[derive(Debug)]
struct Rectangle { length: f64, height: f64 }

#[allow(dead_code)]
struct Triangle { length: f64, height: f64 }

fn print_debug<T: Debug>(t: &T) {
    println!("{:?}", t);
}

fn area<T: HasArea>(t: &T) -> f64 { t.area() }

// empty

struct Cardinal;
struct BlueJay;
struct Turkey;

trait Red {}
trait Blue {}
impl Red for Cardinal {}
impl Blue for BlueJay {}

fn red<T:Red>(_:&T) -> &'static str {"red"}
fn blue<T:Blue>(_:&T) -> &'static str {"blue"}

// multi

use std::fmt::{Display};

// where

trait PrintInOption {
    fn print_in_option(self);
}

impl<T> PrintInOption for T where Option<T>:Debug {
    fn print_in_option(self) {
        println!("{:?}",Some(self));
    }
}

// assoc item issue

struct Container(i32,i32);


// TODO: report the issue
trait Contains<A,B> {
    fn contains(&self,&A,&B) -> bool;
    fn first(&self) -> i32;
    fn last(&self) -> i32;
}

impl Contains<i32,i32> for Container {
    fn contains(&self,number_1:&i32,number_2:&i32) -> bool {
        (&self.0 == number_1) && (&self.1 == number_2)
    }
    fn first(&self) -> i32 {
        self.0
    }
    fn last(&self) -> i32 {
        self.1
    }
}

fn difference<A,B,C>(container: &C) -> i32 where C: Contains<A,B> {
    container.last() - container.first()
}

// assoc item types

struct Container1(i32,i32);

trait Contains1 {
    type A;
    type B;
    fn contains(&self,&Self::A,&Self::B) -> bool;
    fn first(&self) -> i32;
    fn last(&self) -> i32;
}

impl Contains1 for Container1 {
    type A = i32;
    type B = i32;
    fn contains(&self, number_1: &i32, number_2: &i32) -> bool {
        (&self.0 == number_1) && (&self.1 == number_2)
    }
    fn first(&self) -> i32 { self.0 }
    fn last(&self) -> i32 { self.1 }
}

fn difference1<C:Contains1>(container:&C) -> i32 {
    container.last() - container.first()
}

// phantom
use std::marker::PhantomData;
#[derive(PartialEq)]
struct PhantomTuple<A,B>(A,PhantomData<B>);

#[derive(PartialEq)]
struct PhantomStruct<A,B> {
    first: A, phantom: PhantomData<B>,
}

// unit

use std::ops::Add;

#[derive(Debug,Clone,Copy)]
enum Inch {}
#[derive(Debug,Clone,Copy)]
enum Mm {}

#[derive(Debug,Clone,Copy)]
struct Length<Unit>(f64,PhantomData<Unit>);

impl<Unit> Add for Length<Unit> {
    type Output = Length<Unit>;
    fn add(self,rhs:Length<Unit>) -> Length<Unit> {
        Length(self.0+rhs.0,PhantomData)
    }
}

fn main() {
    // structure
    let _s = Single(A);
    let _char: SingleGen<char> = SingleGen('a');
    let _t = SingleGen(A);
    let _i32 = SingleGen(6);
    let _char = SingleGen('a');
    // gen_fn
    reg_fn(Single(A));
    gen_spec_t(SingleGen(A));
    generic::<char>(SingleGen('a'));
    generic(SingleGen('a'));
    // impl
    let x = Val { val: 3.0 };
    let y = GenVal { gen_val: 3i32 };
    println!("{}, {}", x.value(), y.value());
    // gen_trait
    let empty = Empty;
    let null = Null;
    empty.double_drop(null);
    // bounds
    let rectangle = Rectangle { length: 3.0, height: 4.0 };
    let _triangle = Triangle { length: 3.0, height: 4.0 };
    print_debug(&rectangle);
    println!("Area: {}", area(&rectangle));
    // empty
    let cardinal = Cardinal;
    let blue_jay = BlueJay;
    let _turkey = Turkey;
    println!("A cardinal is {}", red(&cardinal));
    println!("A blue jay is {}", blue(&blue_jay));
    // multi
    fn compare_prints<T:Debug + Display>(t: &T) {
        println!("Debug: `{:?}`",t);
        println!("Display `{}`",t);
    }
    fn compare_types<T: Debug, U: Debug>(t: &T, u: &U) {
        println!("t: `{:?}", t);
        println!("u: `{:?}", u);
    }
    let string = "words";
    let array = [1, 2, 3];
    let vec = vec![1, 2, 3];
    compare_prints(&string);
    compare_types(&array, &vec);
    // where
    let vec = vec![1,2,3];
    vec.print_in_option();
    // assoc item issue
    let number_1 = 3;
    let number_2 = 10;
    let container = Container(number_1, number_2);
    println!("Does container contain {} and {}: {}",
             &number_1, &number_2,
             container.contains(&number_1, &number_2));
    println!("First number: {}", container.first());
    println!("Last number: {}", container.last());
    println!("The difference is: {}", difference(&container));
    // assoc item types
    let number_1 = 3;
    let number_2 = 10;

    let container = Container1(number_1, number_2);

    println!("Does container contain {} and {}: {}",
             &number_1, &number_2,
             container.contains(&number_1, &number_2));
    println!("First number: {}", container.first());
    println!("Last number: {}", container.last());

    println!("The difference is: {}", difference1(&container));
    // phantom
    let _tuple1: PhantomTuple<char,f32> = PhantomTuple('Q', PhantomData);
    let _tuple2: PhantomTuple<char,f32> = PhantomTuple('Q', PhantomData);
    let _struct1: PhantomStruct<char, f32> = PhantomStruct {
        first: 'Q',
        phantom: PhantomData,
    };
    let _struct2: PhantomStruct<char, f64> = PhantomStruct {
        first: 'Q',
        phantom: PhantomData,
    };
    // unit
    let one_foot: Length<Inch> = Length(12.0, PhantomData);
    let one_meter: Length<Mm> = Length(1000.0, PhantomData);
    let two_feet = one_foot + one_foot;
    let two_meters = one_meter + one_meter;
    println!("one foot + one_foot = {:?} in", two_feet.0);
    println!("one meter + one_meter = {:?} mm", two_meters.0);
}