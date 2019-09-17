#![allow(dead_code)]
#[derive(Debug)]
enum Food {
    Apple, Carrot, Potato
}

#[derive(Debug)]
struct Peeled(Food);

#[derive(Debug)]
struct Chopped(Food);

#[derive(Debug)]
struct Cooked(Food);

fn peel(food: Option<Food>) -> Option<Peeled> {
    match food {
        Some(food) => Some(Peeled(food)),
        None => None,
    }
}

fn chop(peeled: Option<Peeled>) -> Option<Chopped> {
    match peeled {
        Some(Peeled(food)) => Some(Chopped(food)),
        None               => None,
    }
}

fn cook(chopped: Option<Chopped>) -> Option<Cooked> {
    chopped.map(|Chopped(food)| Cooked(food))
}

fn process(food: Option<Food>) -> Option<Cooked> {
    food.map(|f| Peeled(f))
        .map(|Peeled(f)| Chopped(f))
        .map(|Chopped(f)| Cooked(f))
}

fn eat(food: Option<Cooked>) {
    match food {
        Some(food) => println!("Mmm. I love {:?}",food),
        None => println!("Oh no! It wasn't edible."),
    }
}

// panic
fn give_princess(gift: &str) {
    if gift == "snake" {
        panic!("AAAaaaaa!!!!");
    }
    println!("I love {}s!!!!", gift);
}

// option unwrap

fn give_commoner(gift: Option<&str>) {
    match gift {
        Some("snake") => println!("Yuck! I'm throwing that snake in a fire."),
        Some(inner)   => println!("{}? How nice.", inner),
        None          => println!("No gift? Oh well."),
    }
}

fn give_princess_unwrap(gift: Option<&str>) {
    let inside = gift.unwrap();
    if inside == "snake" {
        panic!("AAAaaaaa!!!!");
    }
    println!("I love {}s!!!!!", inside);
}

// and_then

#[derive(Debug)] enum Day { Monday, Tuesday, Wednesday }
#[derive(Debug)] enum Recipe {
    CordonBleu, Steak, Sushi,
}
fn have_ingredients(food:Recipe) -> Option<Recipe> {
    match food {
        Recipe::Sushi => None,
        _ => Some(food)
    }
}

fn have_recipe(food:Recipe) -> Option<Recipe> {
    match food {
        Recipe::CordonBleu => None,
        _                => Some(food),
    }
}

fn cookable_v1(food:Recipe) -> Option<Recipe> {
    match have_ingredients(food) {
        None => None,
        Some(food) => match have_recipe(food) {
            None       => None,
            Some(food) => Some(food),
        },
    }
}

fn cookable_v2(food:Recipe) -> Option<Recipe> {
    have_ingredients(food).and_then(have_recipe)
}

fn eat_v2(food:Recipe,day: Day) {
    match cookable_v2(food) {
        Some(food) => println!("Yay! On {:?} we get to eat {:?}.", day, food),
        None       => println!("Oh no. We don't get to eat on {:?}?", day),
    }
}

// result

fn double_number(number_str: &str) -> i32 {
    2 * number_str.parse::<i32>().unwrap()
}

// define error type

use std::fmt;
use std::num::ParseIntError;
type Result2<T> = std::result::Result<T,DoubleError>;

#[derive(Debug)]
enum DoubleError {
    EmptyVec,
    Parse(ParseIntError),
}

impl fmt::Display for DoubleError {
    fn fmt(&self, f:&mut fmt::Formatter) -> fmt::Result {
        match *self {
            DoubleError::EmptyVec => write!(f,""),
            DoubleError::Parse(ref e) => e.fmt(f),
        }
    }
}

fn main() {
    // panic
    give_princess("teddy bear");
    // TODO: give_princess("snake");
    // option unwrap
    let food = Some("chicken");
    let snake = Some("snake");
    let void = None;
    give_commoner(food);
    give_commoner(snake);
    give_commoner(void);
    let bird = Some("robin");
    let _nothing: Option<&str> = None;
    give_princess_unwrap(bird);
    // TODO: give_princess_unwrap(_nothing);
    let apple = Some(Food::Apple);
    let carrot = Some(Food::Carrot);
    let potato = None;
    let cooked_apple = cook(chop(peel(apple)));
    let cooked_carrot = cook(chop(peel(carrot)));
    let cooked_potato = process(potato);
    eat(cooked_apple);
    eat(cooked_carrot);
    eat(cooked_potato);
    // and then
    let (cordon_bleu, steak, sushi) = (Recipe::CordonBleu, Recipe::Steak, Recipe::Sushi);
    eat_v2(cordon_bleu, Day::Monday);
    eat_v2(steak, Day::Tuesday);
    eat_v2(sushi, Day::Wednesday);
    // result
    let twenty = double_number("10");
    // TODO: let tt = double_number("t");
    println!("double is {:?}", twenty);
    // TODO: println!("double is {}", tt);
    use std::num::ParseIntError;
    fn double_number(number_str: &str) -> Result<i32, ParseIntError> {
        match number_str.parse::<i32>() {
            Ok(n) => Ok(2 * n),
            Err(e) => Err(e),
        }
    }
    fn double_number_map(number_str: &str) -> Result<i32, ParseIntError> {
        number_str.parse::<i32>().map(|n| 2 * n)
    }
    fn print(result: Result<i32, ParseIntError>) {
        match result {
            Ok(n) => println!("n is {}", n),
            Err(e) => println!("Error: {}", e)
        };
    }
    let twenty = double_number("10");
    print(twenty);
    let tt = double_number_map("t");
    print(tt);
    // result alias
    // ...
    // multiple error types or return early or try!
    type SpecResult<T> = std::result::Result<T,String>;
    fn double_first(vec: Vec<&str>) -> SpecResult<i32> {
        vec.first()
            .ok_or("Please use a vector with at least one element.".to_string())
            .and_then(|s| s.parse::<i32>()
                .map_err(|e| e.to_string())
                .map(|i| 2*i)
            )
    };
    fn print1(result: SpecResult<i32>) {
        match result {
            Ok(n)  => println!("The first doubled is {}", n),
            Err(e) => println!("Error: {}", e),
        }
    }
    let empty = vec![];
    let strings = vec!["tofu","93","18"];
    print1(double_first(empty));
    print1(double_first(strings));
    // define error type
    fn double_first_v3(vec:Vec<&str>) -> Result2<i32> {
        vec.first()
            .ok_or(DoubleError::EmptyVec)
            .and_then(|s| s.parse::<i32>()
                .map_err(DoubleError::Parse)
                .map(|i| 2*i))
    }
    let numbers = vec!["93", "18"];
    let empty = vec![];
    let strings = vec!["tofu", "93", "18"];

    print1(double_first(numbers));
    print1(double_first(empty));
    print1(double_first(strings));
    // reenter try
    impl From<ParseIntError> for DoubleError {
        fn from(err:ParseIntError) -> DoubleError {
            DoubleError::Parse(err)
        }
    }

    fn double_first_3(vec:Vec<&str>)->Result2<i32> {
        let first = try!(vec.first().ok_or(DoubleError::EmptyVec));
        let parsed = try!(first.parse::<i32>());
        Ok(2 * parsed)
    }
    let numbers = vec!["93", "18"];
    let empty = vec![];
    let strings = vec!["tofu", "93", "18"];

    print1(double_first(numbers));
    print1(double_first(empty));
    print1(double_first(strings));
    // boxing errors
    use std::error;
    type Result4<T> = std::result::Result<T,Box<error::Error>>;
    impl error::Error for DoubleError {
        fn description(&self) -> &str {
            match *self {
                DoubleError::EmptyVec => "empty vectors not allowed",
                DoubleError::Parse(ref e) => e.description(),
            }
        }
        fn cause(&self) -> Option<&error::Error> {
            match *self {
                DoubleError::EmptyVec => None,
                DoubleError::Parse(ref e) => Some(e),
            }
        }
    }
    let numbers = vec!["93", "18"];
    let empty = vec![];
    let strings = vec!["tofu", "93", "18"];


    fn double_first4(vec: Vec<&str>) -> Result4<i32> {
        let first = try!(vec.first().ok_or(DoubleError::EmptyVec));
        let parsed = try!(first.parse::<i32>());

        Ok(2 * parsed)
    }

    fn print4(result: Result4<i32>) {
        match result {
            Ok(n)  => println!("The first doubled is {}", n),
            Err(e) => println!("Error: {}", e),
        }
    }

    print4(double_first4(numbers));
    print4(double_first4(empty));
    print4(double_first4(strings));
}