#[macro_use(run_test)]
extern crate interpreter;

#[test]
fn simple_assignment() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            x = 1;
            y = true;
        "
    );
}

#[test]
fn constant_arithmetic() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            x = -(1+1)*(2/(3-8)); // 0.8
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
            a = true || false;
            b = true && false;
        "
    );
}

#[test]
fn failing_test() {
    //panic!();
}
