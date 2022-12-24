use crate::error::Error;
use crate::value::{value, Procedure, Symbol, Value, ValueRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

pub type EnvRef = Rc<RefCell<Env>>;
pub type WeakEnvRef = Weak<RefCell<Env>>;

#[derive(Debug)]
pub struct Env {
    map: HashMap<Symbol, ValueRef>,
    parent: WeakEnvRef,
    children: Vec<EnvRef>,
}

macro_rules! get_reducer {
    ($f:path) => {{
        let f: Procedure = |list| {
            if list.len() == 0 {
                Err(Error::ExpectedAtLeastNArguments(1))
            } else {
                list.iter()
                    .skip(1)
                    .try_fold(list[0].clone(), |a, b| $f(a, b.clone()))
            }
        };
        value!(f)
    }};
}

macro_rules! get_compare_adjecent {
    ($f:path) => {{
        let f: Procedure = |list| {
            if list.len() == 0 {
                Err(Error::ExpectedAtLeastNArguments(1))
            } else {
                for s in list.as_slice().windows(2) {
                    if !$f(s[0].clone(), s[1].clone())?.bool()? {
                        return Ok(value!(false));
                    }
                }
                Ok(value!(true))
            }
        };
        value!(f)
    }};
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
                ("cons".into(), value!(Value::cons as Procedure)),
                ("car".into(), value!(Value::car as Procedure)),
                ("cdr".into(), value!(Value::cdr as Procedure)),
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
        self.children
            .retain(|child| Rc::strong_count(child) == 1 && Rc::weak_count(child) == 0);
    }

    pub fn new(map: HashMap<Symbol, ValueRef>, parent: Weak<RefCell<Env>>) -> EnvRef {
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

    pub fn get(&self, key: &Symbol) -> Result<ValueRef, Error> {
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

    pub fn insert(&mut self, key: Symbol, value: ValueRef) {
        self.map.insert(key, value);
    }
}
