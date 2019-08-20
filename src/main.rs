mod codegen;
mod parser;
use crate::parser::ast::{Parser, Visibility};
use crate::parser::lexer::Lexer;
use crate::parser::resolver::parse_module;
use std::fs;

// mod type_checker;

use crate::parser::ast::BinaryOp;
use crate::parser::ast::Expression;

fn eval(expr: &Expression) -> f64 {
    use BinaryOp::*;
    use Expression::*;
    match expr {
        Int(i) => *i as f64,
        Float(f) => *f,
        Binary(op, a, b) => match op {
            Plus => eval(a.as_ref()) + eval(b.as_ref()),
            Minus => eval(a.as_ref()) - eval(b.as_ref()),
            Times => eval(a.as_ref()) * eval(b.as_ref()),
            Divide => eval(a.as_ref()) / eval(b.as_ref()),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn trim_margin(s: &str) -> String {
    let margin = s
        .split("\n")
        .map(|line| {
            if line.chars().all(|ch| ch.is_whitespace()) {
                return std::usize::MAX;
            }
            let mut index = 0;
            for ch in line.chars() {
                if ch.is_whitespace() {
                    index += 1;
                } else {
                    break;
                }
            }
            index
        })
        .fold(std::usize::MAX, |min, cur_margin| {
            usize::min(min, cur_margin)
        });
    s.split("\n")
        .map(|s| {
            if s.chars().all(|ch| ch.is_whitespace()) {
                ""
            } else {
                &s[margin..]
            }
        })
        .fold(String::new(), |mut s, line| {
            s.push_str(line);
            s.push('\n');
            s
        })
}

fn print_with_pointer(s: &str, ptr: &parser::Span) {
    for (line, text) in s.lines().enumerate() {
        println!("{}", text);
        if line + 1 == ptr.row {
            for _ in 0..ptr.col - 1 {
                print!(" ");
            }
            for _ in 0..ptr.len {
                print!("^");
            }
            println!("");
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let file = fs::read_to_string("./test.rsc")?;
    // println!("{}", file);
    // let mut lexer = Lexer::new(&file);
    // let tokens = lexer.tokens()?;
    // println!("{}", tokens.len());
    // println!(
    //     "{:#?}",
    //     lexer
    //         .tokens()?
    //         .into_iter()
    //         .map(|token| token.kind)
    //         .collect::<Vec<_>>()
    // );
    // let module = resolve(Visibility::Public, "<main>", "./test.rsc").unwrap();
    // println!("{:#?}", module);
    // Ok(())

    // let mut lexer = Lexer::new("<Module foo=\"bar\" bar=20>child</Module>");
    // println!(
    //     "{:?}",
    //     lexer
    //         .tokens()?
    //         .into_iter()
    //         .map(|token| token.kind)
    //         .collect::<Vec<_>>()
    // );

    let module = parse_module("./rsc_src")?;
    let compiled = crate::codegen::compiler::compile_module(&module)?;
    println!("{}", compiled);

    // let prg = "func square(x: Int): Int = { if x then x * x else y }";
    // let expr = Parser::new(Lexer::new(prg)).parse_statement()?;
    // let compiled = crate::codegen::compiler::compile_statement(&expr)?;
    // println!("{}", compiled);

    Ok(())
}
