use crate::value::{Symbol, Value, ValueRef};

#[derive(Debug)]
pub enum Error {
    ExpectedNArguments(usize),
    ExpectedAtLeastNArguments(usize),
    ExpectedBool(Value),
    ExpectedList(Value),
    ExpectedProcedure(ValueRef),
    ExpectedSymbol(Value),
    IncompatibleTypes(&'static str, ValueRef, ValueRef),
    InvalidSyntax(Value),
    Undefined(Symbol),
    Empty,
}
