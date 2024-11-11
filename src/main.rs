use lc::eval::*;
use lc::term::*;

/// Driver code to run the lambda calculus evaluator.
fn main() {
    // (λx. x y) z
    let input = app(abs("x", app(var("x"), var("y"))), var("z"));

    let app_x = app(var("x"), var("x"));
    let app_f = app(var("f"), app_x);
    let abs_x = abs("x", app_f);
    let app_xx = app(abs_x.clone(), abs_x);
    let abs_f = abs("f", app_xx);
    let input = app(abs_f, var("g"));

    println!("Original term: {}", input);
    let result = eval(&input);
    println!("Evaluated term: {}", result);
}
