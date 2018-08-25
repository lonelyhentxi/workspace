struct Nil;
struct Pair(i32,f32);
struct Point {
    x: f32,
    y: f32,
}

#[allow(dead_code)]
struct Rectangle {
    p1: Point,
    p2: Point,
}

fn rect_area(rec:Rectangle) -> f32 {
    let Rectangle { p1: Point { x: p1_x, y: p1_y }, p2: Point { x: p2_x, y: p2_y } } = rec;
    return (p1_x - p2_x).abs()*(p2_y-p1_y).abs();
}

fn main() {
    let point: Point = Point { x: 0.3, y: 0.4 };
    println!("point coordinates: ({} {})", point.x, point.y);
    let Point { x: my_x,y: my_y } = point;
    let _rectangle = Rectangle {
        p1: Point {x:my_x,y:my_y},p2:point,
    };
    let _nil = Nil;
    let pair = Pair(1,0.1);
    println!("pair contains {:?} and {:?}", pair.0, pair.1);
    let Pair(integer, decimal) = pair;
    println!("pair contains {:?} and {:?}", integer, decimal);
    println!("rectangle area of _rectangle is {}",
             rect_area(Rectangle{p1:Point{x:0.0,y:0.0},p2:Point{x:1.0,y:1.0}}));
}