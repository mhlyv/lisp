mod env;
mod error;
mod eval;
mod lambda;
mod parser;
mod value;

use parser::Parser;

fn main() {
    let parser = if let Some(filename) = std::env::args().nth(1) {
        Parser::new(std::fs::File::open(&filename).unwrap_or_else(|_| panic!("couldn't open '{}'", filename)))
    } else {
        Parser::new(std::io::stdin())
    }
    .unwrap();

    let env = env::EnvRef::default();

    for expr in parser {
        match expr {
            Ok(expr) => match eval::eval(expr, env.clone()) {
                Ok(val) => println!("{}", val),
                Err(err) => println!("error: {:?}", err),
            },
            Err(err) => println!("parse error: {:?}", err),
        }
    }
}
