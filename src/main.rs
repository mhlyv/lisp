mod error;
mod eval;
mod lambda;
mod env;
mod value;

use value::value;

fn main() {
    let env = env::EnvRef::default();

    let v = value![
        value!("begin"),
        value![
            value!("define"),
            value!("fib"),
            value![
                value!("lambda"),
                value![value!("n"),],
                value![
                    value!("if"),
                    value![value!(">"), value!(2), value!("n")],
                    value!("n"),
                    value![
                        value!("+"),
                        value![
                            value!("fib"),
                            value![value!("-"), value!("n"), value!(1)],
                        ],
                        value![
                            value!("fib"),
                            value![value!("-"), value!("n"), value!(2)],
                        ],
                    ],
                ],
            ],
        ],
        value![value!("fib"), value!(10)],
    ];

    println!("{}", v);
    println!("{}", eval::eval(v, env).unwrap());
}
