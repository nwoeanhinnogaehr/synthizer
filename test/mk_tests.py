import glob
import json
files = glob.glob("*.test")
print(open("test_base", "r").read())
num = 0
for file in files:
    f = open(file, "r")
    text = f.read()
    f.close()
    [meta, synt] = text.split("###")
    synt = synt.strip()
    meta = json.loads(meta)
    if "compile" in meta:
        print("#[test]")
        if not meta["compile"]:
            print("#[should_fail]")
    print("fn _{}() {{".format(file.split(".")[0]))
    print('let program = "{}";'.format(synt))
    print("let idmap = identifier::IdMap::new();")
    print("let tokens = lexer::lex(program, &idmap).unwrap_or_else(|x| panic!(format!(\"{}\", x)));")
    print("let scope = scope::Scope::new();")
    print("let _: functiondef::FunctionDef = parser::Parser::parse(parser::TokenStream::new(tokens.as_slice()), Cow::Borrowed(&scope)).unwrap_or_else(|x| panic!(format!(\"{}\", x)));")
    print("}")
