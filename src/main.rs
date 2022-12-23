use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::{Rc, Weak};

type Bool = bool;
type Float = f64;
type Integer = i64;
type ValueRef = Rc<Value>;
type Symbol = String;
type List = Vec<ValueRef>;
type Procedure = fn(List) -> Result<ValueRef, Error>;

#[derive(Clone, Debug)]
struct Lambda {
    parameters: Vec<Symbol>,
    body: ValueRef,
    env: WeakEnvRef,
}

impl Lambda {
    fn new(parameters: Vec<Symbol>, body: ValueRef, env: WeakEnvRef) -> Lambda {
        Lambda {
            parameters,
            body,
            env,
        }
    }

    fn eval(&self, args: List) -> Result<ValueRef, Error> {
        let arg_mappings = self
            .parameters
            .clone()
            .into_iter()
            .zip(args.into_iter())
            .collect();

        let new = Env::new(arg_mappings, self.env.clone());

        eval(self.body.clone(), new)
    }
}

impl PartialEq for Lambda {
    fn eq(&self, other: &Self) -> bool {
        self.parameters == other.parameters
            && self.body == other.body
            && self.env.upgrade() == other.env.upgrade()
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Bool(Bool),
    Float(Float),
    Integer(Integer),
    Lambda(Lambda),
    List(List),
    Nil,
    Procedure(Procedure),
    Symbol(String),
}

macro_rules! symbol {
    ($e:expr) => {
        ValueRef::new(Value::Symbol($e.into()))
    };
}

macro_rules! bool {
    ($e:expr) => {
        ValueRef::new(Value::Bool($e))
    };
}

macro_rules! integer {
    ($e:expr) => {
        ValueRef::new(Value::Integer($e))
    };
}

macro_rules! float {
    ($e:expr) => {
        ValueRef::new(Value::Float($e))
    };
}

macro_rules! lambda {
    ($e:expr) => {
        ValueRef::new(Value::Lambda($e))
    };
}

macro_rules! list {
    ($($e:tt)*) => {
        ValueRef::new(Value::List(vec![$($e)*]))
    };
}

macro_rules! nil {
    () => {
        ValueRef::new(Value::Nil)
    };
}

macro_rules! procedure {
    ($e:expr) => {
        ValueRef::new(Value::Procedure($e))
    };
}

#[derive(Debug)]
enum Error {
    ExpectedAtLeastNArguments(usize),
    ExpectedBool(Value),
    ExpectedList(Value),
    ExpectedProcedure(ValueRef),
    ExpectedSymbol(Value),
    IncompatibleTypes(&'static str, ValueRef, ValueRef),
    InvalidSyntax(Value),
    Undefined(Symbol),
}

impl Value {
    fn symbol(&self) -> Result<Symbol, Error> {
        match self {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(Error::ExpectedSymbol(self.clone())),
        }
    }

    fn list(&self) -> Result<List, Error> {
        match self {
            Value::List(list) => Ok(list.clone()),
            _ => Err(Error::ExpectedList(self.clone())),
        }
    }

    fn bool(&self) -> Result<Bool, Error> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(Error::ExpectedBool(self.clone())),
        }
    }

    fn eq(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        Ok(bool!(match (&*a, &*b) {
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

    fn gt(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        match (&*a, &*b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(bool!(a > b)),
            (Value::Float(a), Value::Float(b)) => Ok(bool!(a > b)),
            _ => Err(Error::IncompatibleTypes(">", a, b)),
        }
    }

    fn gte(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        match (&*a, &*b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(bool!(a >= b)),
            (Value::Float(a), Value::Float(b)) => Ok(bool!(a >= b)),
            _ => Err(Error::IncompatibleTypes(">=", a, b)),
        }
    }

    fn lt(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        match (&*a, &*b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(bool!(a < b)),
            (Value::Float(a), Value::Float(b)) => Ok(bool!(a < b)),
            _ => Err(Error::IncompatibleTypes("<", a, b)),
        }
    }

    fn lte(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        match (&*a, &*b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(bool!(a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(bool!(a <= b)),
            _ => Err(Error::IncompatibleTypes("<=", a, b)),
        }
    }

    fn add(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        match (&*a, &*b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(integer!(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(float!(a + b)),
            _ => Err(Error::IncompatibleTypes("+", a, b)),
        }
    }

    fn sub(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        match (&*a, &*b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(integer!(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(float!(a - b)),
            _ => Err(Error::IncompatibleTypes("-", a, b)),
        }
    }

    fn mul(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        match (&*a, &*b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(integer!(a * b)),
            (Value::Float(a), Value::Float(b)) => Ok(float!(a * b)),
            _ => Err(Error::IncompatibleTypes("*", a, b)),
        }
    }

    fn div(a: ValueRef, b: ValueRef) -> Result<ValueRef, Error> {
        match (&*a, &*b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(integer!(a / b)),
            (Value::Float(a), Value::Float(b)) => Ok(float!(a / b)),
            _ => Err(Error::IncompatibleTypes("/", a, b)),
        }
    }
}

macro_rules! get_reducer {
    ($f:path) => {
        procedure!(|list| {
            if list.len() == 0 {
                Err(Error::ExpectedAtLeastNArguments(1))
            } else {
                list.iter()
                    .skip(1)
                    .try_fold(list[0].clone(), |a, b| $f(a, b.clone()))
            }
        })
    };
}

macro_rules! get_compare_adjecent {
    ($f:path) => {
        procedure!(|list| {
            if list.len() == 0 {
                Err(Error::ExpectedAtLeastNArguments(1))
            } else {
                for s in list.as_slice().windows(2) {
                    if !$f(s[0].clone(), s[1].clone())?.bool()? {
                        return Ok(bool!(false));
                    }
                }
                Ok(bool!(true))
            }
        })
    };
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Nil => write!(f, "Nil"),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Float(fl) => write!(f, "{}", fl),
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

type EnvRef = Rc<RefCell<Env>>;
type WeakEnvRef = Weak<RefCell<Env>>;

#[derive(Debug)]
struct Env {
    map: HashMap<Symbol, ValueRef>,
    parent: WeakEnvRef,
    children: Vec<EnvRef>,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            map: HashMap::from([
                ("+".into(), get_reducer!(Value::add)),
                ("-".into(), get_reducer!(Value::sub)),
                ("*".into(), get_reducer!(Value::mul)),
                ("/".into(), get_reducer!(Value::div)),
                ("=".into(), get_compare_adjecent!(Value::eq)),
                (">".into(), get_compare_adjecent!(Value::gt)),
                (">=".into(), get_compare_adjecent!(Value::gte)),
                ("<".into(), get_compare_adjecent!(Value::lt)),
                ("<=".into(), get_compare_adjecent!(Value::lte)),
            ]),
            parent: Default::default(),
            children: Default::default(),
        }
    }
}

impl PartialEq for Env {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Env {
    fn collect(&mut self) {
        // println!("unfiltered {}", self.children.len());

        self.children
            .retain(|child| Rc::strong_count(child) == 1 && Rc::weak_count(child) == 0);

        // println!("filtered {}", self.children.len());
    }

    fn new(map: HashMap<Symbol, ValueRef>, parent: Weak<RefCell<Env>>) -> EnvRef {
        if let Some(p) = parent.upgrade() {
            (*p).borrow_mut().collect()
        }

        let env = Rc::new(RefCell::new(Env {
            map,
            parent: parent.clone(),
            children: Default::default(),
        }));

        if let Some(p) = parent.upgrade() {
            (*p).borrow_mut().children.push(env.clone());
        }

        env
    }

    fn get(&self, key: &Symbol) -> Result<ValueRef, Error> {
        self.map.get(key).cloned().map_or_else(
            || {
                self.parent.upgrade().map_or_else(
                    || Err(Error::Undefined(key.clone())),
                    |v| v.borrow().get(key),
                )
            },
            Ok,
        )
    }

    fn insert(&mut self, key: Symbol, value: ValueRef) {
        self.map.insert(key, value);
    }
}

fn handle_define(list: &List, env: EnvRef) -> Result<ValueRef, Error> {
    if list.len() != 3 {
        return Err(Error::InvalidSyntax(Value::List(list.clone())));
    }

    let key = list[1].symbol()?;

    (*env)
        .borrow_mut()
        .insert(key, eval(list[2].clone(), env.clone())?);

    Ok(nil!())
}

fn handle_begin(list: &List, env: EnvRef) -> Result<ValueRef, Error> {
    if list.len() == 1 {
        return Ok(nil!());
    }

    for e in list[1..list.len() - 1].iter() {
        eval(e.clone(), env.clone())?;
    }

    eval(list[list.len() - 1].clone(), env)
}

fn handle_lambda(list: &List, env: EnvRef) -> Result<ValueRef, Error> {
    if list.len() != 3 {
        return Err(Error::InvalidSyntax(Value::List(list.clone())));
    }

    let parameter_list = list[1].list()?;
    let mut parameters = Vec::new();

    for e in parameter_list.iter() {
        parameters.push(e.symbol()?);
    }

    let body = list[2].clone();
    let env = Rc::downgrade(&env);

    Ok(lambda!(Lambda::new(parameters, body, env)))
}

fn handle_if(list: &List, env: EnvRef) -> Result<ValueRef, Error> {
    if list.len() != 4 {
        return Err(Error::InvalidSyntax(Value::List(list.clone())));
    }

    if *eval(list[1].clone(), env.clone())? == Value::Bool(true) {
        eval(list[2].clone(), env)
    } else {
        eval(list[3].clone(), env)
    }
}

fn eval(expr: ValueRef, env: EnvRef) -> Result<ValueRef, Error> {
    match &*expr {
        Value::Symbol(s) => (*env).borrow().get(s),
        Value::List(list) => {
            if let Value::Symbol(s) = &*list[0] {
                match s.as_str() {
                    "define" => return handle_define(list, env),
                    "begin" => return handle_begin(list, env),
                    "lambda" => return handle_lambda(list, env),
                    "if" => return handle_if(list, env),
                    _ => {}
                }
            }

            let evaluated = eval(list[0].clone(), env.clone())?;

            let mut args = Vec::new();

            for e in list.clone().into_iter().skip(1) {
                args.push(eval(e, env.clone())?);
            }

            match &*evaluated {
                Value::Procedure(proc) => proc(args),
                Value::Lambda(lambda) => lambda.eval(args),
                _ => Err(Error::ExpectedProcedure(evaluated)),
            }
        }
        _ => Ok(expr),
    }
}

fn main() {
    let env = EnvRef::default();

    let v = list![
        symbol!("begin"),
        list![
            symbol!("define"),
            symbol!("fib"),
            list![
                symbol!("lambda"),
                list![symbol!("n")],
                list![
                    symbol!("if"),
                    list![symbol!(">"), integer!(2), symbol!("n")],
                    symbol!("n"),
                    list![
                        symbol!("+"),
                        list![
                            symbol!("fib"),
                            list![symbol!("-"), symbol!("n"), integer!(1)],
                        ],
                        list![
                            symbol!("fib"),
                            list![symbol!("-"), symbol!("n"), integer!(2)],
                        ],
                    ],
                ],
            ],
        ],
        list![symbol!("fib"), integer!(10)],
    ];

    println!("{}", v);
    println!("{}", eval(v, env).unwrap());
}
