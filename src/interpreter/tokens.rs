use super::ident::Identifier;

use std::fmt;
use std::ops::Deref;

pub type Number = f32;

/// The various types that a token can be
#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Token {
    Ident(Identifier),
    Const(Number),
    Boolean(bool),
    Operator(Operator),
    Symbol(Symbol),
}

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    Mod,
    Less,
    Greater,
    Equal,
    NotEqual,
    ApproxEqual,
    Not,
    And,
    Or,
    Xor,
    GreaterEqual,
    LessEqual,
}

#[derive(PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

impl Operator {
    pub fn parse(s: &str) -> Option<Operator> {
        use self::Operator::*;
        Some(match s {
            "+" => Add,
            "-" => Sub,
            "*" => Mul,
            "/" => Div,
            "^" => Exp,
            "%" => Mod,
            "==" => Equal,
            "!=" => NotEqual,
            "~=" => ApproxEqual,
            "<" => Less,
            ">" => Greater,
            "<=" => LessEqual,
            ">=" => GreaterEqual,
            "!" => Not,
            "&&" => And,
            "||" => Or,
            "^^" => Xor,
            _ => return None,
        })
    }
    pub fn precedence(&self) -> i32 {
        use self::Operator::*;
        match *self {
            And | Or | Xor => 10,
            Equal | NotEqual | ApproxEqual => 20,
            Less | Greater | GreaterEqual | LessEqual => 30,
            Add | Sub => 40,
            Mul | Div | Mod => 50,
            Not | Exp => 60,
        }
    }

    pub fn can_take_x_args(&self, x: i32) -> bool {
        use self::Operator::*;
        match *self {
            Not => x == 1,
            Sub => x == 1 || x == 2,
            _ => x == 2,
        }
    }

    pub fn associativity(&self) -> Associativity {
        use self::Operator::*;
        match *self {
            Exp => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Symbol {
    Period,
    Comma,
    Equals,
    Colon,
    Semicolon,
    QuestionMark,
    Backslash,
    If,
    Else,
    LeftBracket(Bracket),
    RightBracket(Bracket),
}

impl Symbol {
    pub fn parse(s: &str) -> Option<Symbol> {
        use self::Symbol::*;
        Some(match s {
            "." => Period,
            "," => Comma,
            "=" => Equals,
            ":" => Colon,
            ";" => Semicolon,
            "?" => QuestionMark,
            "\\" => Backslash,
            "if" => If,
            "else" => Else,
            "(" => LeftBracket(Bracket::Round),
            ")" => RightBracket(Bracket::Round),
            "{" => LeftBracket(Bracket::Curly),
            "}" => RightBracket(Bracket::Curly),
            "[" => LeftBracket(Bracket::Square),
            "]" => RightBracket(Bracket::Square),
            _ => return None,
        })
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Symbol::*;
        let string = match *self {
            Period => ".",
            Comma => ",",
            Equals => "=",
            Colon => ":",
            Semicolon => ";",
            QuestionMark => "?",
            Backslash => "\\",
            If => "if",
            Else => "else",
            LeftBracket(Bracket::Round) => "(",
            RightBracket(Bracket::Round) => ")",
            LeftBracket(Bracket::Curly) => "{",
            RightBracket(Bracket::Curly) => "}",
            LeftBracket(Bracket::Square) => "[",
            RightBracket(Bracket::Square) => "]",
        };
        write!(f, "{}", string)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match *self {
            Ident(x) => write!(f, "Id({})", x),
            Operator(x) => write!(f, "{:?}", x),
            Const(x) => write!(f, "{}", x),
            Symbol(x) => write!(f, "{}", x),
            Boolean(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug, Copy, PartialEq, Clone)]
pub enum Bracket {
    Round,
    Square,
    Curly,
}

#[derive(Copy, Clone, PartialEq)]
pub struct SourcePos {
    pub line: usize,
    pub column: usize,
    pub index: usize,
    pub line_index: usize, //index of first character of line
}

impl SourcePos {
    pub fn new() -> SourcePos {
        SourcePos {
            line: 1,
            column: 1,
            index: 0,
            line_index: 0,
        }
    }
    pub fn anon() -> SourcePos {
        SourcePos {
            line: 0,
            column: 0,
            index: 0,
            line_index: 0,
        }
    }
    /// Requires col to have reached the end of line for indices to be properly incremented.
    pub fn add_line(&mut self) {
        self.line += 1;
        self.column = 1;
        self.index += 1; // newline
        self.line_index = self.index;
    }

    pub fn add_chars(&mut self, num: usize) {
        self.column += num;
        self.index += num;
    }

    pub fn is_anon(&self) -> bool {
        self.line == 0
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_anon() {
            write!(f, "<anon>")
        } else {
            write!(f, "{}:{}", self.line, self.column)
        }
    }
}

impl fmt::Debug for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_anon() {
            write!(f, "<anon>")
        } else {
            write!(f, "{}:{}/idx={}", self.line, self.column, self.index)
        }
    }
}

pub struct Node<T>(pub T, pub SourcePos);

pub trait NodeImpl<'a> {
    type Item;
    type Pos;
    fn item(&'a self) -> <Self as NodeImpl>::Item;
    fn pos(&'a self) -> <Self as NodeImpl>::Pos;
}

impl<'a, T> NodeImpl<'a> for Node<T> {
    type Item =  &'a T;
    type Pos = SourcePos;
    fn item(&'a self) -> &'a T {
        &self.0
    }
    fn pos(&self) -> SourcePos {
        self.1
    }
}

impl<'a, T> NodeImpl<'a> for Option<Node<T>> where T: Copy {
    type Item =  Option<T>;
    type Pos = Option<SourcePos>;
    fn item(&self) -> Option<T> {
        self.map(|x| x.0)
    }
    fn pos(&self) -> Option<SourcePos> {
        self.map(|x| x.1)
    }
}

impl<T> fmt::Debug for Node<T> where T: fmt::Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple(&format!("Node+{}", self.1))
            .field(&self.0)
            .finish()
    }
}

impl<T> fmt::Display for Node<T> where T: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} @ {}", self.0, self.1)
    }
}

impl<T> Copy for Node<T> where T: Copy { }

impl<T> Clone for Node<T> where T: Clone {
    fn clone(&self) -> Node<T> {
        Node(self.0.clone(), self.1)
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref<'a>(&'a self) -> &'a T {
        &self.0
    }
}

