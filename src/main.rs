use lc::eval::*;
use lc::term::*;

/// Driver code to run the lambda calculus evaluator.
fn main() {
    let tmp = abs("x", app(var("f"), app(var("x"), var("x"))));
    let input = abs("f", app(tmp.clone(), tmp));

    println!("Original term: {}", input);
    let result = eval(&eval(&input));
    println!("Evaluated term: {}", result);
}
