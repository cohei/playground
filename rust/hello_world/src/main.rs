extern crate hello_world;

use hello_world::english::greetings;

use std::ops::Add;

fn main() {
    let f: fn(i32) -> i32 = plus_one;

    let s: &str = "Hello, world!";
    let (i,): (i32,) = (42,);
    println!("{}, {}",  s, f(i));

    let foo = Foo { x: &7 };
    println!("x is: {}", foo.x());

    let hd = HasDrop;

    println!("Hello in English: {}", greetings::hello());

    println!("fact 10: {}", fact(10));

    collatz(25);
    collatz_while(25);

    // println!("double + {}", double(|x, y| x + y, 1));
    println!("double + {}", double(Add::add, 1));

    Peano::Zero == Peano::Succ(Box::new(Peano::Zero));
}

fn plus_one(i: i32) -> i32 {
    i + 1
}

fn collatz(n: usize) {
    let v = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };

    println!("{}", v);

    if v != 1 { collatz(v) }
}

fn collatz_while(n_: usize) {
    let mut n = n_;

    while n != 1 {
        n = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };

        println!("{}", n);
    }
}

fn diverges() -> ! {
    panic!("This function never returns!");
}

fn test() {
    let v = vec!(1,2,3);

    take(&v);

    println!("v[0] is: {}", v[0]);
}

fn take(v: &Vec<i32>) {
    // ここで何が起きるかは重要ではない
}

struct Foo<'a> {
    x: &'a i32
}

impl<'a> Foo<'a> {
    fn x(&self) -> &'a i32 { self.x }
}

use std::ops::Mul;

trait HasArea<T> {
    fn area(&self) -> T;
}

#[derive(Debug)]
struct Circle<T> {
    x: T,
    y: T,
    radius: T
}

impl<T> HasArea<T> for Circle<T> where T : Mul<Output=T> + Copy {
    fn area(&self) -> T {
        self.radius * self.radius
    }
}

fn print_area<T: HasArea<f64>>(shape: T) {
    println!("This shape has an area of {}", shape.area());
}

struct HasDrop;

impl Drop for HasDrop {
    fn drop(&mut self) {
        println!("Dropping!");
    }
}

fn fact(n: i64) -> i64 {
    if n == 0 {
        1
    } else {
        n * fact(n - 1)
    }
}

enum MyOption<T> {
    None,
    Some(T)
}

fn is_some<T>(x: MyOption<T>) -> bool {
    match x {
        MyOption::None => false,
        MyOption::Some(_) => true
    }
}

fn double<F>(f: F, x: i64) -> i64 where F: Fn(i64, i64) -> i64 {
    f(x, x)
}

trait Testable {
    fn test(&self) -> bool;
}

impl Testable for i64 {
    fn test(&self) -> bool {
        *self != 0
    }
}

enum Peano {
    Zero,
    Succ(Box<Peano>)
}

impl PartialEq for Peano {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Peano::Zero, &Peano::Zero) => true,
            (&Peano::Zero, _) => false,
            (_, &Peano::Zero) => false,
            (&Peano::Succ(ref a), &Peano::Succ(ref b)) => a == b,
        }
    }
}
