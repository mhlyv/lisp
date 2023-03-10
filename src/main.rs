mod env;
mod error;
mod eval;
mod lambda;
mod parser;
mod value;

use parser::Parser;

fn interactive() {
    let parser = Parser::new(std::io::stdin()).unwrap();
    let env = env::EnvRef::default();

    for expr in parser {
        match expr {
            Ok(expr) => match eval::eval(expr, env.clone()) {
                Ok(val) => println!("{val}"),
                Err(err) => println!("error: {err:?}"),
            },
            Err(err) => println!("parse error: {err:?}"),
        }
    }
}

fn eval(filename: String) {
    let parser = Parser::new(
        std::fs::File::open(&filename).unwrap_or_else(|_| panic!("couldn't open '{filename}'")),
    )
    .unwrap();
    let env = env::EnvRef::default();

    for expr in parser {
        match expr {
            Ok(expr) => match eval::eval(expr, env.clone()) {
                Ok(val) => println!("=> {val}"),
                Err(err) => println!("error: {err:?}"),
            },
            Err(err) => println!("parse error: {err:?}"),
        }
    }
}

fn main() {
    if let Some(filename) = std::env::args().nth(1) {
        eval(filename)
    } else {
        interactive()
    }
}
