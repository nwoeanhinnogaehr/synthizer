#[derive(Debug)]
pub enum Expr<'input> {
	Number(f64),
    Identifier(&'input str),
	Infix(Box<Expr<'input>>, Infix, Box<Expr<'input>>),
    Prefix(Prefix, Box<Expr<'input>>),
    Block(Block<'input>),
    Closure(Function<'input>),
    FunctionCall(FunctionCall<'input>),
}

#[derive(Debug)]
pub enum Infix {
	Mul,
	Div,
	Add,
	Sub,
	Mod,
	Pow,
}

#[derive(Debug)]
pub enum Prefix {
    Neg,
    Plus,
    Not,
}

#[derive(Debug)]
pub struct Function<'input> {
    pub ident: Option<&'input str>,
    pub args: ArgsDef<'input>,
    pub block: Block<'input>,
}

pub type ArgsDef<'input> = Vec<ArgumentDef<'input>>;
pub type ArgsRef<'input> = Vec<ArgumentRef<'input>>;

#[derive(Debug)]
pub struct ArgumentDef<'input> {
    pub ident: &'input str,
    pub default: Option<Box<Expr<'input>>>,
}

#[derive(Debug)]
pub enum ArgumentRef<'input> {
    Expr(Box<Expr<'input>>),
    Assign(Assignment<'input>),
    OpAssign(OpAssignment<'input>),
}

pub type Block<'input> = Vec<Statement<'input>>;

#[derive(Debug)]
pub enum Statement<'input> {
    Expr(Box<Expr<'input>>),
    Assignment(Assignment<'input>),
}

#[derive(Debug)]
pub struct Assignment<'input> {
    pub ident: &'input str,
    pub expr: Box<Expr<'input>>,
}

#[derive(Debug)]
pub struct OpAssignment<'input> {
    pub ident: &'input str,
    pub op: Infix,
    pub expr: Box<Expr<'input>>,
}

#[derive(Debug)]
pub struct FunctionCall<'input> {
    pub callee: Box<Expr<'input>>,
    pub args: ArgsRef<'input>,
}

#[derive(Debug)]
pub enum Item<'input> {
    Assignment(Assignment<'input>),
    Function(Function<'input>),
}
