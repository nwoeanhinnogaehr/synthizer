#[macro_use]
extern crate interpreter;

#[test]
fn reassign_different_type() {
    run_test!(
        should_warn(typecheck)
        => r"
            x = 1;
            x = true;
        ");
}

#[test]
fn shadow_function() {
    run_test!(
        should_warn(typecheck)
        => r"
            fn x { x }
            fn x { x*2 }
        ");
}

#[test]
fn call_non_function() {
    run_test!(
        should_fail(typecheck)
        => r"
            x = 1;
            y = x(1, 2, 3);
        ");
}

#[test]
fn wrong_num_arguments() {
    run_test!(
        should_fail(typecheck)
        => r"
            f x, y=1 { x+y }
            x = f(1, 2, 3);
        ");
    run_test!(
        should_fail(typecheck)
        => r"
            f x, y=1 { x+y }
            x = f();
        ");
}

#[test]
fn misspelled_argument() {
    run_test!(
        should_fail(typecheck)
        => r"
            fn x { x }
            x = fn(y=5);
        ");
}

#[test]
fn ambiguous_recursion() {
    run_test!(
        should_fail(typecheck)
        => r"
            fn x { fn(x) }
            y = fn(1);
        ");
}

#[test]
fn wrong_arg_type() {
    run_test!(
        should_fail(typecheck)
        => r"
            fn x { x }
            x = fn(1);
            y = fn(true);
        ");
    run_test!(
        should_fail(typecheck)
        => r"
            fn x { x }
            z = fn;
            x = z(1);
            y = fn(true);
        ");
}

#[test]
fn wrong_block_type() {
    run_test!(
        should_fail(typecheck)
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
        should_fail(typecheck)
        => r"
            x = true if 1 else false;
        ");
}

#[test]
fn reassignment() {
    run_test!(
        should_pass(typecheck)
        => r"
            x = true;
            y = x ^^ true;
            x = 1;
            y = x * 5;
        ");
    run_test!(
        should_pass(typecheck)
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
        should_fail(typecheck)
        => r"
            x = a;
        ");
    run_test!(
        should_fail(typecheck)
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
        should_fail(typecheck)
        => r"
            x = 1 if true else false;
        ");
}

#[test]
fn wrong_type_numerical_op() {
    run_test!(
        should_fail(typecheck)
        => r"
            a = 5 + true;
        ");
}

#[test]
fn wrong_type_comparison_op() {
    run_test!(
        should_fail(typecheck)
        => r"
            a = 5 + true;
        ");
}

#[test]
fn wrong_type_equality_op() {
    run_test!(
        should_fail(typecheck)
        => r"
            a = 5 == true;
        ");
}

#[test]
fn wrong_type_boolean_op() {
    run_test!(
        should_fail(typecheck)
        => r"
            a = 5 && true;
        ");
}

#[test]
fn wrong_type_prefix_op() {
    run_test!(
        should_fail(typecheck)
        => r"
            a = -true;
        ");
    run_test!(
        should_fail(typecheck)
        => r"
            a = !5;
        ");
}

#[test]
fn internal_block_shadowing_captured_vars() {
    run_test!(
        should_pass(typecheck)
        => r"
            x = 5;
            c = \{x+5};
            z = {
                x = true;
                c();
            }
        ");
}

#[test]
fn default_args() {
    run_test!(
        should_pass(typecheck)
        => r"
            fn x, y=3 { x + y }
            a = fn(1);
            a = fn(1, 2);
            a = fn(1, x=2);
        ");
}

#[test]
fn partial_application() {
    run_test!(
        should_pass(typecheck)
        => r"
            fn x, y=3 { x + y }
            fn' = fn{x=2};
            a = fn'(1);
            a = fn'(1, 2);
            a = fn'(1, x=2);
        ");
}

#[test]
fn implicit_call() {
    run_test!(
        should_pass(typecheck)
        => r"
            fn x, y=3 { x + y }
            x = 1;
            y = false;
            z = fn[];
        ");
    run_test!(
        should_fail(typecheck)
        => r"
            fn x, y { x + y }
            x = 1;
            y = false;
            z = fn[];
        ");
}

#[test]
fn unbind_expression() {
    run_test!(
        should_fail(typecheck)
        => r"
            fn x, y=3 { x + y }
            fn' = fn{y=};
            x = 1;
            y = false;
            z = fn'[];
        ");
}
