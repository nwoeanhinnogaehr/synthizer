use super::issue::IssueTracker;
use super::symbol::SymbolTable;
use super::parser::Parser;
use super::typecheck::TypeChecker;
use super::lexer;
use std::fs::File;
use std::io::Read;

pub struct Synth<'a> {
    filename: &'a str,
    code: String,
}

impl<'a> Synth<'a> {
    pub fn new(filename: &'a str) -> Result<Synth<'a>, String> {
        let code = try!(read_file(filename));
        Ok(Synth {
            filename: filename,
            code: code,
        })
    }
    pub fn compile(&'a mut self) -> bool {
        let (symtab, issues);
        symtab = SymbolTable::new();
        issues = IssueTracker::new(self.filename, &self.code);
        self.front_end(&symtab, &issues);
        println!("{}", issues);
        return issues.is_ok();
    }
    fn front_end(&'a self, symtab: &'a SymbolTable<'a>, issues: &'a IssueTracker<'a>) -> bool {
        let tokens = match lexer::lex(&issues, &self.code, &symtab) {
            Some(t) => t,
            None => return false,
        };
        let mut parser = Parser::new(tokens, &issues);
        let ast = match parser.parse() {
            Some(a) => a,
            None => return false,
        };
        let typechecker = TypeChecker::new(&ast, &symtab, &issues);
        let types_ok = typechecker.check();
        if !types_ok {
            return false;
        }

        return true;
    }
}

fn read_file(filename: &str) -> Result<String, String> {
    let mut file = match File::open(filename) {
        Err(why) => {
            return Err(format!("couldn't open {}: {}", filename, why));
        }
        Ok(file) => file,
    };
    let mut code = String::new();
    match file.read_to_string(&mut code) {
        Err(why) => {
            return Err(format!("couldn't read {}: {}", filename, why));
        }
        Ok(_) => { }
    }
    return Ok(code);
}
