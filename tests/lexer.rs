#[macro_use(run_test)]
extern crate interpreter;

#[test]
fn all_tokens() {
    run_test!(
        should_pass(lex)
        => r"
            1
            1.1
            .1
            1.
            1.1e5
            1e5
            .1e5
            1.e5
            1E5

            abcABC_~'0123

            + - * / ^ ^^ >= <= < > ! % && || == !=
            if else . , = : ; ? ( ) { } [ ] \
            true false
            // #&*GR^@&(G#^&(G@&*YFD*B@Y^&#(VT@^(f367g9@&*
        "
    );
}

#[test]
fn non_tokens() {
    run_test!(
        should_fail(lex)
        => "` @ # $ & | ' \""
    );
}
