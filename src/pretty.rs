use crate::term::*;
use std::fmt;

/// Pretty prints a term.
pub fn pretty_print(term: &Term) -> String {
    match term {
        term @ Term::Var(_) => pretty_print_precedenc(term, 10),
        term @ Term::Abs(_, _) => pretty_print_precedenc(term, 2),
        term @ Term::App(_, _) => pretty_print_precedenc(term, 3),
    }
}

// For how this works, see https://stackoverflow.com/questions/27471937/showsprec-and-operator-precedences/43639618#43639618
fn pretty_print_precedenc(term: &Term, p: u32) -> String {
    match term {
        Term::Var(identifier) => identifier.into(),
        Term::Abs(identifier, term) => {
            let r = pretty_print_precedenc(term, 3);
            show_paren(p > 3, format!("λ{identifier}. {r}"))
        }
        Term::App(l, r) => {
            let l = pretty_print_precedenc(l, 4);
            let r = pretty_print_precedenc(r, 5);
            show_paren(p > 4, format!("{l} {r}"))
        }
    }
}

fn show_paren(show: bool, s: String) -> String {
    if show {
        format!("({s})")
    } else {
        s
    }
}

/// Display trait implementation for Term.
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", pretty_print(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_var() {
        let term = var("x");
        let expected = "x";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_single_abs() {
        let term = abs("x", var("x"));
        let expected = "λx. x";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_single_app() {
        let term = app(abs("x", var("x")), var("y"));
        let expected = "(λx. x) y";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_complex() {
        let term = app(abs("x", app(var("x"), var("y"))), var("z"));
        let expected = "(λx. x y) z";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_more_complex() {
        let term = app(app(abs("x", abs("y", var("x"))), var("a")), var("b"));
        let expected = "(λx. λy. x) a b";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_two_arguments() {
        let term = abs("x", abs("y", app(var("x"), var("y"))));
        let expected = "λx. λy. x y";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
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
        let expected = "(λx. λy. λz. y) a b c";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn blub() {
        let term = app(app(abs("x", abs("y", var("x"))), var("a")), var("b"));
        let expected = "(λx. λy. x) a b";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn blub2() {
        let term = app(
            app(abs("x", var("x")), var("a")),
            app(abs("x", var("x")), var("b")),
        );
        let expected = "(λx. x) a ((λx. x) b)";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_y_combinator() {
        let tmp = abs("x", app(var("f"), app(var("x"), var("x"))));
        let term = abs("f", app(tmp.clone(), tmp));
        let expected = "λf. (λx. f (x x)) (λx. f (x x))";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }
}
