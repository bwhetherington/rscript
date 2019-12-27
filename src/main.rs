#![allow(unused)]

// mod codegen;
mod engine;
mod parser;

// mod type_checker;
// use crate::codegen::compiler::Compiler;
use crate::{
    engine::{Engine, Value},
    parser::{parse_module, BinaryOp, Expression, Lexer, Module, Parser, Statement, Visibility},
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

use std::io::{self, prelude::*};

struct Console {
    line: String,
}

impl Console {
    pub fn new() -> Console {
        Console {
            line: String::new(),
        }
    }

    pub fn prompt<'a, 'b>(&'a mut self, prompt: &'b str) -> io::Result<&'a str> {
        self.line.clear();
        print!("{}", prompt);
        io::stdout().flush()?;
        io::stdin().read_line(&mut self.line)?;
        self.line = self.line.trim().to_string();
        Ok(&self.line)
    }
}

fn handle_input(engine: &mut Engine, input: &str) -> Result<Value, Box<dyn Error>> {
    let input = format!("{};", input);
    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);

    let statement = parser.parse_statement()?;
    match statement {
        Statement::Expression(expr) => Ok(engine.evaluate(&expr)?),
        other => {
            engine.execute(&other)?;
            Ok(Value::None)
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut engine = Engine::new();
    engine.init()?;

    parse_ast("rsc_src")?;
    let module = parse_module("rsc_src")?;
    engine.load_module(&module, true).expect("failed");

    // Find the main method
    engine.run_main().expect("failed");
    // match engine.run_main()? {
    //     Value::Number(n) if (n as i32 as f64) == n => {
    //         let n = n as i32;
    //         // std::process::exit(n);
    //     }
    //     other => std::process::exit(-1),
    // }

    // Ok(())

    let mut console = Console::new();
    loop {
        let input = console.prompt("> ")?;

        if input == "exit" {
            break;
        }

        match handle_input(&mut engine, input) {
            Ok(value) if value.type_of() != "None" => {
                engine.print_value(&value)?;
                println!();
            }
            Ok(_) => (),
            Err(why) => println!("error: {}", why),
        }
    }

    Ok(())
}
