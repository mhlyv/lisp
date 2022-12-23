use std::rc::Rc;
use crate::value::{Value, ValueRef, List, value};
use crate::lambda::Lambda;
use crate::env::EnvRef;
use crate::error::Error;

fn handle_define(list: &List, env: EnvRef) -> Result<ValueRef, Error> {
    if list.len() != 3 {
        return Err(Error::InvalidSyntax(Value::List(list.clone())));
    }

    let key = list[1].symbol()?;

    (*env)
        .borrow_mut()
        .insert(key, eval(list[2].clone(), env.clone())?);

    Ok(value!())
}

fn handle_begin(list: &List, env: EnvRef) -> Result<ValueRef, Error> {
    if list.len() == 1 {
        return Ok(value!());
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

    Ok(value!(Lambda::new(parameters, body, env)))
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

pub fn eval(expr: ValueRef, env: EnvRef) -> Result<ValueRef, Error> {
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

