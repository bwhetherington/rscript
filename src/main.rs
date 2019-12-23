#![allow(unused)]

// mod codegen;
mod engine;
mod parser;

// mod type_checker;
// use crate::codegen::compiler::Compiler;
use crate::{
    engine::{Engine, Value},
    parser::{parse_module, BinaryOp, Expression, Lexer, Module, Parser, Visibility},
};
use std::error::Error;
use std::fs;
use std::process::Command;

// const js_source: &'static str = include_str!("./include.js");

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

// fn compile_module(module: &str) -> Result<(), Box<dyn Error>> {
//     let module = parse_ast(module)?;
//     let root = "rsc_module";
//     let mut compiler = Compiler::new(root);
//     let compiled = compiler.compile_module(&module)?;
//     let program = format!("{}{}={}{}.main.main();", js_source, root, compiled, root);
//     fs::write("out.js", program)?;

//     Ok(())
// }

#[allow(unused)]
fn parse_ast(module: &str) -> Result<Module, Box<dyn Error>> {
    let module = parse_module(module)?;
    let data = format!("{:#?}", module);
    fs::write("./output.ast", &data)?;
    Ok(module)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut engine = Engine::new();
    engine.init();

    let module = parse_module("rsc_src")?;
    engine.load_module(&module, true).unwrap();

    // Find the main method
    engine.run_main().unwrap();

    Ok(())
}
