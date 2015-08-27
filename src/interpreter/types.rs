use super::ident::Identifier;
use super::scope::ScopedTable;

use vec_map::VecMap;
use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Type {
    Number,
    Boolean,
    Function(Identifier),

    /// With recursive functions, it may not be possible to tell exactly what the type is without
    /// further information. This is different from a None type, which means that this is a
    /// logically inconsistent or indeterminable type. The user should never see this.
    Indeterminate,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Number => write!(f, "Number"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Function(_) => write!(f, "Function"),
            Type::Indeterminate => write!(f, "Indeterminate"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionType {
    pub args: VecMap<Type>, // From Identifier to Type
    pub returns: Type,
}

impl FunctionType {
    pub fn new(args: VecMap<Type>, returns: Type) -> FunctionType {
        FunctionType {
            args: args,
            returns: returns,
        }
    }
}

pub type TypeTable = ScopedTable<Type>;
