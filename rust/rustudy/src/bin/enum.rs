#![allow(dead_code)]

enum Person {
    Engineer,
    Scientist,
    Height(i32),
    Weight(i32),
    Info {name:String,height:i32}
}

enum Number {
    Zero,
    One,
    Two,
}

enum Color {
    Red = 0xff0000,
    Green = 0x00ff00,
    Blue = 0x0000ff,
}

fn inspect(p:Person) {
    match p {
        Person::Engineer => println!("Is engineer!"),
        Person::Scientist => println!("Is scientist!"),
        Person::Height(i) => println!("Has a height of {}.",i),
        Person::Weight(i) => println!("Has a weight of {}.",i),
        Person::Info {name,height} => {
            println!("{} is  {} tall!",name, height);
        }
    };
}

fn main() {
    let person = Person::Height(18);
    let amira = Person::Weight(10);
    let dave = Person::Info{name: String::from("Dave"), height:72 };
    let rebecca = Person::Scientist;
    let rohan = Person::Engineer;

    inspect(person);
    inspect(amira);
    inspect(dave);
    inspect(rebecca);
    inspect(rohan);

    println!("start show colors:");
    println!("zero is {}",Number::Zero as i32);
    println!("one is {}", Number::One as i32);
    println!("roses are #{:06x}",Color::Red as i32);
    println!("violets are #{:06x}", Color::Blue as i32);
}