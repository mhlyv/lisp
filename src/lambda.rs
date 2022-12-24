use crate::env::{Env, WeakEnvRef};
use crate::error::Error;
use crate::eval::eval;
use crate::value::{List, Symbol, ValueRef};

#[derive(Clone, Debug)]
pub struct Lambda {
    parameters: Vec<Symbol>,
    body: ValueRef,
    env: WeakEnvRef,
}

impl Lambda {
    pub fn new(parameters: Vec<Symbol>, body: ValueRef, env: WeakEnvRef) -> Lambda {
        Lambda {
            parameters,
            body,
            env,
        }
    }

    pub fn eval(&self, args: List) -> Result<ValueRef, Error> {
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
