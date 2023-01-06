use crate::error::Error;
use crate::lambda::Lambda;
use std::fmt::Display;
use std::rc::Rc;

pub type Bool = bool;
pub type Float = f64;
pub type Integer = i64;
pub type ValueRef = Rc<Value>;
pub type Symbol = String;
pub type List = Vec<ValueRef>;
pub type Procedure = fn(List) -> Result<ValueRef, Error>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(Bool),
    Float(Float),
    Integer(Integer),
    Lambda(Lambda),
    List(List),
    Nil,
    Procedure(Procedure),
    Symbol(Symbol),
}

macro_rules! value_from {
    ($i:ident, $t:ty) => {
        impl From<$t> for Value {
            fn from(value: $t) -> Self {
                Self::$i(value.into())
            }
        }
    };
}

value_from!(Bool, Bool);
value_from!(Float, Float);
value_from!(Integer, Integer);
value_from!(Lambda, Lambda);
value_from!(List, List);
value_from!(List, &[ValueRef]);
value_from!(Procedure, Procedure);
value_from!(Symbol, Symbol);
value_from!(Symbol, &str);

macro_rules! value {
    () => {{
        use crate::value::{ValueRef, Value};
        ValueRef::new(Value::Nil)
    }};
    ($e:expr) => {{
        use crate::value::{ValueRef, Value};
        ValueRef::new(Value::from($e))
    }};
    ($($e:expr),+ $(,)?) => {{
        use crate::value::{ValueRef, Value};
        ValueRef::new(Value::from(vec![$($e),+]))
    }}
}

pub(crate) use value;

macro_rules! arithmetic {
    ($name:tt, $op:tt, $repr:expr) => {
        pub fn $name(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
            Ok(match (&*a, &*b) {
                (Value::Integer(a), Value::Integer(b)) => value!(a $op b),
                (Value::Float(a), Value::Float(b)) => value!(a $op b),
                _ => return Err(Error::IncompatibleTypes($repr, a, b)),
            })
        }
    };
}

macro_rules! expect_n_args {
    ($args:expr, $n:expr) => {
        if $args.len() != $n {
            return Err(Error::ExpectedNArguments($n));
        }
    };
}

impl Value {
    pub fn symbol(&self) -> Result<Symbol, Error> {
        match self {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(Error::ExpectedSymbol(self.clone())),
        }
    }

    pub fn list(&self) -> Result<List, Error> {
        match self {
            Value::List(list) => Ok(list.clone()),
            _ => Err(Error::ExpectedList(self.clone())),
        }
    }

    pub fn bool(&self) -> Result<Bool, Error> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(Error::ExpectedBool(self.clone())),
        }
    }

    pub fn eq(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        Ok(value!(match (&*a, &*b) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Lambda(a), Value::Lambda(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Procedure(a), Value::Procedure(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }))
    }

    arithmetic!(gt, >, ">");
    arithmetic!(gte, >=, ">=");
    arithmetic!(lt, <, "<");
    arithmetic!(lte, <=, "<=");
    arithmetic!(add, +, "+");
    arithmetic!(sub, -, "-");
    arithmetic!(mul, *, "*");
    arithmetic!(div, /, "/");

    pub fn car(args: List) -> Result<ValueRef, Error> {
        expect_n_args!(args, 1);
        args[0].list()?.first().ok_or(Error::Empty).cloned()
    }

    pub fn cdr(args: List) -> Result<ValueRef, Error> {
        expect_n_args!(args, 1);

        Ok(value!(&args[0].list()?[1..]))
    }

    pub fn cons(args: List) -> Result<ValueRef, Error> {
        expect_n_args!(args, 2);

        let a = args[0].clone();
        let b = args[1].clone();

        Ok(match &*b {
            Value::Nil => value!(a,),
            Value::List(list) => {
                let mut list = list.clone();
                list.insert(0, a);
                value!(list)
            }
            _ => return Err(Error::ExpectedList((*b).clone())),
        })
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Nil => write!(f, "nil"),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Float(fl) => write!(f, "{fl}"),
            Value::List(l) => {
                if l.is_empty() {
                    write!(f, "()")
                } else {
                    write!(f, "({}", l[0])?;
                    for e in l.iter().skip(1) {
                        write!(f, " {}", *e)?;
                    }
                    write!(f, ")")
                }
            }
            Value::Procedure(_) => write!(f, "Procedure"),
            Value::Lambda(_) => write!(f, "Lambda"),
        }
    }
}
