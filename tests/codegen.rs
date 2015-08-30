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
            a = (-(1+1)*(2/(3-8)))%0.3; // 0.8
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
                x+y*z-w;
            }
            b = a[y=2, z=3, w=4, x=1];
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
            c = b[x=5];
        "
    );
}

#[test]
fn default_args() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a x=2, y { x+y }
            b = a[y=5];
        ");
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            a x, y=2 { x+y }
            b = a[x=5];
            c = a(5);
        ");
}

#[test]
fn factorial() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fact n {
                n*fact[n -= 1] if n > 1 else 1;
            }
            x = fact(10);
        ");
}

#[test]
fn fib() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fib n {
                fib[n -= 1] + fib[n -= 2] if n > 2 else 1;
            }
            x = fib(10);
        ");
}

#[test]
fn closure_default_args() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            foo bar=\y{y*2} { bar }
            a = foo()(2);
        ");
}

#[test]
fn unused_function() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fn x {
                x*5;
            }
        ");
}

// not passing because the testing macro doesn't define any intrinsics
#[test]
fn intrinsics() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            x = 1.5;
            y = sin(x) + cos(x) + sqrt(x) + log(x) + log2(x) + floor(x) + exp(x);
        ");
}

#[test]
fn closure_capture() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            x = 5;
            y = \{x+5};
            z = {
                x = true;
                y();
            };
        ");
}

#[test]
fn return_closure_capture() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fn x {
                z = 5;
                \w=x{w+z};
            }
            y = fn(5);
            z = fn(6);
            x = y() + z(); // x == 21
        ");
}

// it's currently unclear whether this is okay or not.
// if it's an easy fix it'll work soon.
#[test]
fn use_funtion_before_def() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fn1 {
                fn2()
            }
            fn2 {
                42
            }
            x = fn1();
        ");
}

#[test]
fn pass_function() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fiver fn {
                fn(5)
            }

            square x {
                x*x
            }

            x = fiver(square);
        ");
}

#[test]
fn pass_closure() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fiver fn {
                fn(5)
            }

            x = fiver(\x { x*x });
        ");
}

#[test]
fn return_function() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            passthru x {
                x
            }

            squarer {
                \x { x*x }
            }

            square = passthru(squarer());
            x = square(5);
            y = square(6);
        ");
}

#[test]
fn return_closure() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            squarer {
                \x { x*x }
            }

            square = squarer();
            x = square(5);
            y = square(6);
        ");
}

#[test]
fn argument_ordering() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fn1 x, y {
                x if y else -x;
            }
            fn2 y, x {
                x if y else -x;
            }
            fn3 = \x, y {
                x if y else -x;
            };
            fn4 = \y, x {
                x if y else -x;
            };
            a = fn1[x=1, y=true];
            b = fn1(1, true);
            c = fn2[y=true, x=1];
            d = fn2(true, 1);
            e = fn3[x=1, y=true];
            f = fn3(1, true);
            g = fn4[y=true, x=1];
            h = fn4(true, 1);
        ");
}

#[test]
fn complex_closure_capture() {
    run_test!(
        should_pass(lex, parse, typecheck, codegen)
        => r"
            fn x {
                y = 2^x if x < 1 else x^2;
                \n { y%n };
            }
            x = fn(3)(2);
        ");
}
