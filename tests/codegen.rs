#[macro_use(run_test)]
extern crate interpreter;

#[test]
fn simple_assignment() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = 1;
            b = true;
        "
    );
}

#[test]
fn constant_arithmetic() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = -(1+1)*(2/(3-8)); // 0.8
        "
    );
}

#[test]
fn comparisons() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = 1 < 1;
            b = 1 <= 1;
            c = 1 > 1;
            d = 1 >= 1;
            e = 1 == 2;
            f = 1 != 2;
            g = true == false;
            h = true != false;

        "
    );
}

#[test]
fn boolean_arithmetic() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = true || !true;
            b = true && !true;
        "
    );
}

#[test]
fn reassign() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = 1;
            a = 2;
        "
    );
}

#[test]
fn global_ref() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = 1;
            b = a + 1;
        "
    );
}

#[test]
fn conditional() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = 2;
            b = 3;
            c = (a if true else a)
                if a + b < a * b
                else (a if false else b);
            d = true if true else true;
        "
    );
}

#[test]
fn block() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
        a = { true };
        b = { 1; 2; 3 };
        c = { 1; { 2; 3 } };
        "
    );
}
