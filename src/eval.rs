use crate::term::*;
use std::collections::HashSet;

/// Evaluates a term:
/// In order to simplify the term as much as possible, we use a call-by-value strategy.
/// We even evaluate the body of an abstraction eagerly.
///
/// The evaluation rules are as follows:
/// - A variable evaluates to itself.
/// - An abstraction evaluates to itself, even the body is eagerly evaluated.
/// - An application evaluates the left side, then the right side,
///   and applies the left side to the right side by substitution.
///   Examples:
///   `x` evaluates to `x`.
///   `λx. x` evaluates to `λx. x`.
///   `(λx. x) y` evaluates to `y`.
///   `(λx. (λy. x)) z` evaluates to `λy. z`.
///   `(λx. (λy. x)) a b` evaluates to `a`.
pub fn eval(term: &Term) -> Term {
    match term {
        Term::Var(identifier) => var(identifier),
        Term::Abs(identifier, term) => abs(identifier, eval(term)),
        Term::App(term_l, term_r) => {
            let l = eval(term_l);
            let r = eval(term_r);

            if let Term::Abs(var, body) = l {
                substitute(&body, &var, &r)
            } else {
                app(l, r)
            }
        }
    }
}

/// Replace all occurrences of a variable `var` in a `term` with `replacement`.
pub fn substitute(term: &Term, var: &str, replacement: &Term) -> Term {
    match term {
        Term::Var(x) if x == var => replacement.clone(),
        Term::Var(x) => Term::Var(x.clone()),
        Term::Abs(param, body) if param != var => {
            if free_variables(replacement).contains(param) {
                // Prevent variable capture by renaming the parameter
                let fresh_var = fresh_name(param, term, replacement);
                // Rename the parameter in the body
                let new_body = substitute(body, param, &Term::Var(fresh_var.clone()));
                abs(&fresh_var, substitute(&new_body, var, replacement))
            } else {
                abs(param, substitute(body, var, replacement))
            }
        }
        Term::App(t1, t2) => Term::App(
            Box::new(substitute(t1, var, replacement)),
            Box::new(substitute(t2, var, replacement)),
        ),
        _ => term.clone(),
    }
}

/// Collects free variables in a term.
pub fn free_variables(term: &Term) -> HashSet<String> {
    match term {
        Term::Var(x) => {
            let mut set = HashSet::new();
            set.insert(x.clone());
            set
        }
        Term::Abs(param, body) => {
            let mut set = free_variables(body);
            set.remove(param);
            set
        }
        Term::App(t1, t2) => {
            let mut set = free_variables(t1);
            set.extend(free_variables(t2));
            set
        }
    }
}

/// Generates a fresh variable name based on `base_name` that doesn't exist in `existing_vars`.
fn fresh_name(base_name: &str, term: &Term, replacement: &Term) -> String {
    let all_vars: HashSet<String> = collect_all_vars(term)
        .union(&collect_all_vars(replacement))
        .cloned()
        .collect();

    let mut fresh_var = base_name.to_string();
    let mut counter = 1;
    while all_vars.contains(&fresh_var) {
        fresh_var = format!("{}_{}", base_name, counter);
        counter += 1;
    }
    fresh_var
}

/// Collects all variables in a term (free and bound).
fn collect_all_vars(term: &Term) -> HashSet<String> {
    match term {
        Term::Var(x) => {
            let mut vars = HashSet::new();
            vars.insert(x.clone());
            vars
        }
        Term::Abs(param, body) => {
            let mut vars = collect_all_vars(body);
            vars.insert(param.clone());
            vars
        }
        Term::App(t1, t2) => {
            let mut vars = collect_all_vars(t1);
            vars.extend(collect_all_vars(t2));
            vars
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_free_variables() {
        let term = abs("x", app(var("x"), var("y")));
        let free_vars = free_variables(&term);
        let expected_vars: HashSet<_> = vec!["y".to_string()].into_iter().collect();
        assert_eq!(free_vars, expected_vars);
    }

    #[test]
    fn test_substitute_simple() {
        let term = app(var("x"), var("y"));
        let replacement = var("z");
        let substituted = substitute(&term, "x", &replacement);
        let expected = app(var("z"), var("y"));
        assert_eq!(substituted, expected);
    }

    #[test]
    fn test_substitute_capture_avoiding() {
        let term = abs("x", var("y"));
        let replacement = var("x");
        let substituted = substitute(&term, "y", &replacement);
        let expected = abs("x_1", var("x"));
        assert_eq!(substituted, expected);
    }

    #[test]
    fn test_eval_var() {
        // x -> x
        let term = var("x");
        let evaluated = eval(&term);
        let expected = var("x");
        assert_eq!(evaluated, expected);
    }

    #[test]
    fn test_eval_simple_application() {
        // (λx. x) y -> y
        let term = app(abs("x", var("x")), var("y"));
        let evaluated = eval(&term);
        let expected = var("y");
        assert_eq!(evaluated, expected);
    }

    #[test]
    fn test_eval_nested_abstraction() {
        // (λx. λy. x) z -> λy. z
        let term = app(abs("x", abs("y", var("x"))), var("z"));
        let evaluated = eval(&term);
        let expected = abs("y", var("z"));
        assert_eq!(evaluated, expected);
    }

    #[test]
    fn test_free_variables_in_nested_abstraction() {
        let term = abs("x", abs("y", app(var("x"), var("z"))));
        let free_vars = free_variables(&term);
        let expected_vars: HashSet<_> = vec!["z".to_string()].into_iter().collect();
        assert_eq!(free_vars, expected_vars);
    }

    #[test]
    fn test_substitute_in_nested_abstraction() {
        let term = abs("x", app(var("x"), var("y")));
        let replacement = var("z");
        let substituted = substitute(&term, "y", &replacement);
        let expected = abs("x", app(var("x"), var("z")));
        assert_eq!(substituted, expected);
    }

    #[test]
    fn test_eval_complex_application() {
        // (λx. (λy. y) x) z -> z
        let term = app(abs("x", app(abs("y", var("y")), var("x"))), var("z"));
        let evaluated = eval(&term);
        let expected = var("z");
        assert_eq!(evaluated, expected);
    }

    #[test]
    fn test_eval_more_complex_application() {
        // (λx. λy. x) a b -> a
        let term = app(app(abs("x", abs("y", var("x"))), var("a")), var("b"));
        let evaluated = eval(&term);
        let expected = var("a");
        assert_eq!(evaluated, expected);
    }

    #[test]
    fn test_eval_more_complex_application2() {
        // (λx. x) a ((λx. x) b)
        let term = app(
            app(abs("x", var("x")), var("a")),
            app(abs("x", var("x")), var("b")),
        );
        let evaluated = eval(&term);
        let expected = app(var("a"), var("b"));
        assert_eq!(evaluated, expected);
    }

    #[test]
    fn test_three_arguments() {
        let term = app(
            app(
                app(abs("x", abs("y", abs("z", var("y")))), var("a")),
                var("b"),
            ),
            var("c"),
        );
        let evaluated = eval(&term);
        let expected = var("b");
        assert_eq!(evaluated, expected);
    }

    #[test]
    fn test_y_combinator() {
        // λf.    (λx. f (x x)) (λx. f (x x))
        // ->
        // λf. f ((λx. f (x x)) (λx. f (x x)))
        let tmp = abs("x", app(var("f"), app(var("x"), var("x"))));
        let term = abs("f", app(tmp.clone(), tmp));

        let x_abs = abs("x", app(var("f"), app(var("x"), var("x"))));
        let f_app = app(var("f"), app(x_abs.clone(), x_abs));
        let f_abs = abs("f", f_app);

        let expected = f_abs;
        let evaluated = eval(&term);
        assert_eq!(evaluated, expected);
    }
}
