use super::synthizer;
use std::fmt::Debug;

fn should_fail<X: Debug, Y: Debug>(res: Result<X, Y>) {
    if let Ok(x) = res {
        panic!("Expected failure but got {:?}", x);
    }
}

fn should_pass<X: Debug, Y: Debug>(res: Result<X, Y>) {
    if let Err(x) = res {
        panic!("Expected success but got {:?}", x);
    }
}

// the generated code is ~300000 lines and it takes like 5 minutes to compile
// if we make all the parsers public, so to save precious time we just don't
// test the smaller components on their own. for now.
/*#[test]
fn num() {
    should_pass(synthizer::parse_Num("22"));
    should_pass(synthizer::parse_Num("22.23"));
    should_pass(synthizer::parse_Num(".23"));
    should_pass(synthizer::parse_Num("22."));
    should_fail(synthizer::parse_Num("."));
    should_fail(synthizer::parse_Num(""));
    should_fail(synthizer::parse_Num("3.3.3"));
}

#[test]
fn term() {
    should_pass(synthizer::parse_Term("(-22)"));
    should_pass(synthizer::parse_Term("-22.23"));
    should_pass(synthizer::parse_Term("((-.23))"));
    should_pass(synthizer::parse_Term("((-a))"));
    should_pass(synthizer::parse_Term("-22."));
    should_pass(synthizer::parse_Term("22"));
    should_pass(synthizer::parse_Term("a"));
    should_pass(synthizer::parse_Term(".23"));
    should_pass(synthizer::parse_Term("22."));
    should_fail(synthizer::parse_Term("()"));
    should_fail(synthizer::parse_Term("(2))"));
    should_fail(synthizer::parse_Term("((2)"));
}

#[test]
fn expr() {
    should_pass(synthizer::parse_Expr("1 + -2"));
    should_pass(synthizer::parse_Expr("-22.23 * 4"));
    should_pass(synthizer::parse_Expr(".23 / 23.3"));
    should_pass(synthizer::parse_Expr("-22."));
    should_pass(synthizer::parse_Expr("+22.23"));
    should_pass(synthizer::parse_Expr("!x"));
    should_pass(synthizer::parse_Expr("2*a+9*-(b+4+-c)/2"));
    should_fail(synthizer::parse_Expr("22-"));
    should_fail(synthizer::parse_Expr("*.23"));
}

#[test]
fn ident() {
    should_pass(synthizer::parse_Ident("a"));
    should_pass(synthizer::parse_Ident("abc"));
    should_pass(synthizer::parse_Ident("abc123"));
    should_pass(synthizer::parse_Ident("_abc1_3A"));
    should_fail(synthizer::parse_Ident("9"));
    should_fail(synthizer::parse_Ident("9a"));
    should_fail(synthizer::parse_Ident("#"));
}

#[test]
fn block() {
    should_pass(synthizer::parse_Block("{ 1, y=4, y, { x=5, (y+3*{ 1, 2 }) } }"));
    should_pass(synthizer::parse_Block("{}"));
    should_pass(synthizer::parse_Block("{{{{{{{}}}}}}}"));
    should_pass(synthizer::parse_Block("{y=5}"));
    should_fail(synthizer::parse_Block("{y=5k}"));
    should_pass(synthizer::parse_Block("{5}"));
    should_pass(synthizer::parse_Block("{5+5,}"));
    should_fail(synthizer::parse_Block("{,5+5}"));
    should_fail(synthizer::parse_Block("{y=,}"));
}

#[test]
fn function() {
    should_pass(synthizer::parse_Function("a { }"));
    should_pass(synthizer::parse_Function("a x,y,z { }"));
    should_pass(synthizer::parse_Function("a x=3,y,z=4 { }"));
    should_pass(synthizer::parse_Function("a x=3,y,z=4 { }"));
}

#[test]
fn closure() {
    should_pass(synthizer::parse_Closure("${}"));
    should_pass(synthizer::parse_Closure("$x,y,z {x+y+z}"));
    should_pass(synthizer::parse_Closure("$x=3,y,z=4 {}"));
    should_pass(synthizer::parse_Closure("$x=3,y,z=4 {}"));
}

#[test]
fn function_call() {
    should_pass(synthizer::parse_FunctionCall("fn(x, y, z)"));
    should_pass(synthizer::parse_FunctionCall("fn(x, y, z,)"));
    should_pass(synthizer::parse_FunctionCall("fn(x=1, y=2, z)"));
    should_pass(synthizer::parse_FunctionCall("fn(x+=1, y=2, z+=y)"));
    should_pass(synthizer::parse_FunctionCall("fn()"));
    should_fail(synthizer::parse_FunctionCall("fn(1=a)"));
}*/

#[test]
fn grammar() {
    should_pass(synthizer::parse_Grammar(r"
                                     x = 5,
                                     foo a, b=3, c=$ll{ll+1} {
                                        q = -19*(b + {1,2,3,4,c}),
                                        x,
                                        a + b + c,
                                        //(foo(a=4, b, c=3))(a),
                                        $z{z*z}(5),
                                     },
                                     q = $f{f()},
    "));
    should_fail(synthizer::parse_Grammar(r"a = foo(1)(2)"));
    should_pass(synthizer::parse_Grammar(r"a = (foo(1))(2)"));
    should_pass(synthizer::parse_Grammar(r"a = foo((1)(2))"));
}
