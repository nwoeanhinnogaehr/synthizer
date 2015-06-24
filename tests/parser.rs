#[macro_use(run_test)]
extern crate interpreter;

#[test]
fn unclosed_paren() {
    run_test!(
        should_fail(parse)
        => r"
            x = (5 + (5);
        ");
}

#[test]
fn missing_assignment_semicolon() {
    run_test!(
        should_fail(parse)
        => r"
            x = 5
        ");
}

#[test]
fn bad_item() {
    run_test!(
        should_fail(parse)
        => r"
            a;
        ");
    run_test!(
        should_fail(parse)
        => r"
            5 = a;
        ");
}

#[test]
fn empty_assignment_expr() {
    run_test!(
        should_fail(parse)
        => r"
            a = ;
        ");
}

#[test]
fn extra_closing_paren() {
    run_test!(
        should_fail(parse)
        => r"
            a = (5+5));
        ");
}

#[test]
fn unary_operator_used_as_binary() {
    run_test!(
        should_fail(parse)
        => r"
            a = true ! false;
        ");
}

#[test]
fn binary_operator_used_as_unary() {
    run_test!(
        should_fail(parse)
        => r"
            a = 5 + * 5;
        ");
    run_test!(
        should_fail(parse)
        => r"
            a = 5 + 5 -;
        ");
    run_test!(
        should_fail(parse)
        => r"
            a = + 5 + 5;
        ");
}

#[test]
fn closure_missing_block() {
    run_test!(
        should_fail(parse)
        => r"
            a = \x,y,z;
        ");
}

#[test]
fn block_not_closed() {
    run_test!(
        should_fail(parse)
        => r"
            a = \x,y,z { x; { y; z; }
        ");
    run_test!(
        should_fail(parse)
        => r"
            a = { 5 + 5;
        ");
}

#[test]
fn extra_semicolon() {
    run_test!(
        should_fail(parse)
        => r"
            a = 5;;
        ");
    run_test!(
        should_fail(parse)
        => r"
            a = {
                5;
                ;
            };
        ");
}

#[test]
fn empty_program() {
    run_test!(
        should_pass(parse)
        => "");
}

#[test]
fn empty_argument() {
    run_test!(
        should_fail(parse)
        => r"
            a = fn(5,);
        ");
}

#[test]
fn wrong_bracket_type() {
    run_test!(
        should_fail(parse)
        => r"
            a = fn(5];
        ");
    run_test!(
        should_fail(parse)
        => r"
            a = fn{5);
        ");
}

#[test]
fn bad_argument() {
    run_test!(
        should_fail(parse)
        => r"
            a = fn(5=a);
        ");
    run_test!(
        should_fail(parse)
        => r"
            b = fn(=5);
        ");
    run_test!(
        should_fail(parse)
        => r"
            d = fn(=);
        ");
}

#[test]
fn closure_in_default_arg() {
    run_test!(
        should_pass(parse)
        => r"
            fn y=\x{x<5} { }
        ");
    run_test!(
        should_pass(parse)
        => r"
            fn z=\x=\{5},y=\{6}{x()<y()} { z() }
        ");
}

#[test]
fn closure_in_arg() {
    run_test!(
        should_pass(parse)
        => r"
            a = fn(3, cond=\{x>5});
        ");
}

#[test]
fn block_in_default_arg() {
    run_test!(
        should_fail(parse) // hopefully this will be supported at some point for completeness
                           // but right now implementing it is a major pain
        => r"
            fn y=99-{1+2+3+4+5} { }
        ");
}

#[test]
fn block_in_arg() {
    run_test!(
        should_pass(parse)
        => r"
            a = fn(y={1+2+3+4+5});
        ");
}

#[test]
fn partial_application_in_arg() {
    run_test!(
        should_pass(parse)
        => r"
            a = fn(y=fx{5});
            a = fn{y=fx{5}};
        ");
}

#[test]
fn partial_application_in_default_arg() {
    run_test!(
        should_fail(parse) // FIXME
        => r"
            fn x=y{5} { x() }
        ");
}

#[test]
fn closure_with_semicolon() {
    run_test!(
        should_pass(parse)
        => r"
            a = \x, y { x; y; };
        ");
    run_test!(
        should_pass(parse)
        => r"
            a = \x=\c{c;}, y { x; y; };
        ");
}

#[test]
fn call_types() {
    run_test!(
        should_pass(parse)
        => r"
            a = fn(1, 2);
            b = fn(x=1, 2);
            c = fn(2, x=1);
            e = fn(y=1, x=2);
            f = fn[1, 2];
            g = fn[x=1, 2];
            h = fn[2, x=1];
            i = fn[y=1, x=2];
            j = fn{1, 2};
            k = fn{x=1, 2};
            l = fn{2, x=1};
            m = fn{y=1, x=2};
        ");
}

#[test]
fn unbind() {
    run_test!(
        should_pass(parse)
        => r"
            x = fn{y=};
        ");
}

#[test]
fn everything_at_once() {
    run_test!(
        should_fail(parse) // FIXME
        => r"
            z = \x{x*x*x};
            fn a, b=z{5}, c=true, d=\e,f=1,g=\i=\j{2*j}{i();}{e;f*g()} {
                x = a + {
                    b if c else d(c);
                    d{e=a}(f=2);
                    d{b}[3];
                    d{b}{g=\x{x^2}};
                };
            }
            a = fn({5;4;3;2;1;});
        ");
}
