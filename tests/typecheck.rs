#[macro_use]
extern crate interpreter;

#[test]
fn reassign_different_type() {
    run_test!(
        should_warn(typecheck),
        should_pass(lex, parse)
        => r"
            x = 1;
            x = true;
        ");
}

#[test]
fn shadow_function() {
    run_test!(
        should_warn(typecheck),
        should_pass(lex, parse)
        => r"
            fn x { x }
            fn x { x*2 }
        ");
}

#[test]
fn call_non_function() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            x = 1;
            y = x();
        ");
}

#[test]
fn wrong_num_arguments() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            f x, y=1 { x+y }
            x = f(x=1, y=2, z=3);
        ");
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            f x, y=1 { x+y }
            x = f();
        ");
}

#[test]
fn misspelled_argument() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            fn x { x }
            x = fn(y=5);
        ");
}

#[test]
fn ambiguous_recursion() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            fn x { fn(x) }
            y = fn(x=1);
        ");
}

#[test]
fn wrong_arg_type() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            fn x { x }
            x = fn(x=1);
            y = fn(x=true);
        ");
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            fn x { x }
            z = fn;
            x = z(x=1);
            y = fn(x=true);
        ");
}

#[test]
fn wrong_block_type() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            x = {
                true;
                false;
            };
        ");
}

#[test]
fn non_boolean_condition() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            x = true if 1 else false;
        ");
}

#[test]
fn reassignment() {
    run_test!(
        should_pass(lex, parse, typecheck)
        => r"
            x = true;
            y = x ^^ true;
            x = 1;
            y = x * 5;
        ");
    run_test!(
        should_pass(lex, parse, typecheck)
        => r"
            x = true;
            a { x ^^ true }
            x = 1;
            b { x * 2 }
            w = b();
            z = a();
        ");
}

#[test]
fn variable_not_in_scope() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            x = a;
        ");
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            fn {
                a = 5;
            }
            x = a;
        ");
}

#[test]
fn mismatched_conditional_types() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            x = 1 if true else false;
        ");
}

#[test]
fn wrong_type_numerical_op() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            a = 5 + true;
        ");
}

#[test]
fn wrong_type_comparison_op() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            a = 5 + true;
        ");
}

#[test]
fn wrong_type_equality_op() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            a = 5 == true;
        ");
}

#[test]
fn wrong_type_boolean_op() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            a = 5 && true;
        ");
}

#[test]
fn wrong_type_prefix_op() {
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            a = -true;
        ");
    run_test!(
        should_fail(typecheck),
        should_pass(lex, parse)
        => r"
            a = !5;
        ");
}

#[test]
fn internal_block_shadowing_captured_vars() {
    run_test!(
        should_pass(lex, parse, typecheck)
        => r"
            x = 5;
            c = \{x+5};
            z = {
                x = true;
                c();
            };
        ");
}

#[test]
fn default_args_evald_in_new_scope() {
    run_test!(
        should_pass(lex, parse, typecheck)
        => r"
            x = 5;
            fn z=x {
                z^2;
            }
            w = {
                x = true;
                fn();
            };
        ");
}

#[test]
fn default_args() {
    run_test!(
        should_pass(lex, parse, typecheck)
        => r"
            fn x, y=3 { x + y }
            a = fn(x=1);
            a = fn(x=1, y=2);
            a = fn(y=1, x=2);
        ");
}

#[test]
fn recursion_numeric() {
    run_test!(
        should_pass(lex, parse, typecheck)
        => r"
            a b {
                b;
                a(b-=1);
            }
            z = a(b=5);
        ");
}

#[test]
fn call_nonexistant_function() {
    run_test!(
        should_pass(lex, parse),
        should_fail(typecheck)
        => r"
            x = f();
        ");
}

#[test]
fn reference_nonexistant_variable() {
    run_test!(
        should_pass(lex, parse),
        should_fail(typecheck)
        => r"
            x = f;
        ");
}

#[test]
fn call_default_arg() {
    run_test!(
        should_pass(lex, parse, typecheck)
        => r"
            fn d=\e{e} {
                d(e=5);
            }
            a = fn();
        ");
}

#[test]
fn indirect_recursion() {
    run_test!(
        should_pass(lex, parse),
        should_fail(typecheck)
        => r"
            a { b() }
            b { a() }
            x = a();
        ");
}

#[test]
fn unused_function() {
    run_test!(
        should_pass(lex, parse),
        should_warn(typecheck)
        => r"
            f x { x }
        ");
}
