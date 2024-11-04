use crate::term::*;
use std::fmt;

/// Pretty prints a term.
pub fn pretty_print(term: &Term) -> String {
    match term {
        Term::Var(identifier) => identifier.into(),
        Term::Abs(identifier, term) => format!("(λ{identifier}. {term})"),
        Term::App(term_l, term_r) => format!("{term_l} {term_r}"),
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
    fn test_pp_var() {
        let term = var("x");
        let expected = "x";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_pp_abs() {
        let term = abs("x", var("x"));
        let expected = "(λx. x)";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_pp_app() {
        let term = app(abs("x", var("x")), var("y"));
        let expected = "(λx. x) y";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_pp_complex() {
        let term = app(abs("x", app(var("x"), var("y"))), var("z"));
        let expected = "(λx. x y) z";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }

    #[test]
    fn test_pp_more_complex() {
        let term = app(app(abs("x", abs("y", var("x"))), var("a")), var("b"));
        let expected = "(λx. (λy. x)) a b";
        let pp = pretty_print(&term);
        assert_eq!(pp, expected);
    }
}
