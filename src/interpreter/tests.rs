/// should_pass: expects no errors.
/// should_fail: expects errors.
/// should_warn: expects warnings.
#[macro_export]
macro_rules! run_test {
    ( $name:expr,
      $( $prop:ident ( $( $val:ident ),* ) ),*
      => $source:expr ) => {{
        use ::interpreter::common::Context;
        use ::interpreter::parser::parse;
        use ::interpreter::lexer::lex;
        use ::interpreter::typecheck::typecheck;

        use std::collections::HashSet;

        let mut should_pass: HashSet<&'static str> = HashSet::new();
        let mut should_fail: HashSet<&'static str> = HashSet::new();
        let mut should_warn: HashSet<&'static str> = HashSet::new();
        $(
            $(
                match stringify!($prop) {
                    "should_pass" => { should_pass.insert(stringify!($val)); },
                    "should_fail" => { should_fail.insert(stringify!($val)); },
                    "should_warn" => { should_warn.insert(stringify!($val)); },
                    _ => panic!("unknown property"),
                }
            )*
        )*
        let ctxt = Context::new($name.into(), $source.into());

        lex(&ctxt);
        let err = ctxt.issues.borrow().has_errors();
        let warn = ctxt.issues.borrow().has_warnings();
        if should_pass.contains(&"lex") && err {
            panic!("lex should have passed:\n{}", *ctxt.issues.borrow());
        }
        if should_fail.contains(&"lex") && !err {
            panic!("lex should have produced errors:\n{}", *ctxt.issues.borrow());
        }
        if should_warn.contains(&"lex") && !warn {
            panic!("lex should have produced warnings:\n{}", *ctxt.issues.borrow());
        }
        ctxt.issues.borrow_mut().clear();

        parse(&ctxt);
        let err = ctxt.issues.borrow().has_errors();
        let warn = ctxt.issues.borrow().has_warnings();
        if should_pass.contains(&"parse") && err {
            panic!("parse should have passed:\n{}", *ctxt.issues.borrow());
        }
        if should_fail.contains(&"parse") && !err {
            panic!("parse should have produced errors:\n{}", *ctxt.issues.borrow());
        }
        if should_warn.contains(&"parse") && !warn {
            panic!("parse should have produced warnings:\n{}", *ctxt.issues.borrow());
        }
        ctxt.issues.borrow_mut().clear();

        typecheck(&ctxt);
        let err = ctxt.issues.borrow().has_errors();
        let warn = ctxt.issues.borrow().has_warnings();
        if should_pass.contains(&"typecheck") && err {
            panic!("typecheck should have passed:\n{}", *ctxt.issues.borrow());
        }
        if should_fail.contains(&"typecheck") && !err {
            panic!("typecheck should have produced errors:\n{}", *ctxt.issues.borrow());
        }
        if should_warn.contains(&"typecheck") && !warn {
            panic!("typecheck should have produced warnings:\n{}", *ctxt.issues.borrow());
        }
        ctxt.issues.borrow_mut().clear();
    }};
}
