use crate::value::{Value, ValueRef, Symbol};

#[derive(Debug)]
pub enum Error {
    ExpectedAtLeastNArguments(usize),
    ExpectedBool(Value),
    ExpectedList(Value),
    ExpectedProcedure(ValueRef),
    ExpectedSymbol(Value),
    IncompatibleTypes(&'static str, ValueRef, ValueRef),
    InvalidSyntax(Value),
    Undefined(Symbol),
}
