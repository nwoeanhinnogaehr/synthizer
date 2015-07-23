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
            c = true ^^ !true;
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

#[test]
fn block_assignment() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = {
                b = true;
                5 if b else 6;
                b = !b;
                5 if b else 6;
            };
        "
    );
}

#[test]
fn function_def() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a x, y, z, w {
                x;
                y;
                z;
                w;
            }
            b = a(y=2, z=3, w=4, x=1);
        "
    );
}

#[test]
fn closure() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a = \x { x+5 };
            b = a;
            c = b(x=5);
        "
    );
}

#[test]
fn default_args() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a x=2, y { x+y }
            b = a(y=5);
        ");
}

#[test]
fn factorial() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fact n {
                n*fact(n -= 1) if n > 1 else 1;
            }
            x = fact(n=10);
        ");
}

#[test]
fn fib() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fib n {
                fib(n -= 1) + fib(n -= 2) if n > 2 else 1;
            }
            x = fib(n=10);
        ");
}
