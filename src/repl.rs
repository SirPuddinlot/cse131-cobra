use crate::helpers::*;

// src/repl.rs
use std::io::{self, Write, BufRead};
use im::HashMap;
use std::mem;
use dynasmrt::{DynasmApi, dynasm};
use crate::ast::*;
use crate::parser::*;
use crate::jit::*;


pub fn run_repl() -> io::Result<()> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let mut defines: HashMap<String, i32> = HashMap::new();
    
    let stdin = io::stdin();
    let mut reader = stdin.lock();
    
    loop {
        print!("> ");
        io::stdout().flush()?;
        
        let mut input = String::new();
        reader.read_line(&mut input)?;
        
        let input = input.trim();
        
        // Check for exit commands
        if input == "exit" || input == "quit" {
            break;
        }
        
        if input.is_empty() {
            continue;
        }
        
        // Parse the input
        let sexp = match sexp::parse(input) {
            Ok(s) => s,
            Err(_e) => {
                println!("Invalid: parse error");
                continue;
            }
        };
        
        // Parse into ReplEntry
        // let entry = match parse_repl_entry(&sexp, 0) {
        //     Ok(e) => e,
        //     Err(msg) => {
        //         println!("{}", msg);
        //         continue;
        //     }
        // };
        // Parse into ReplEntry - catch panics from parser
        let entry = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            parse_repl_entry(&sexp, 0)
        })) {
            Ok(Ok(e)) => e,
            Ok(Err(msg)) => {
                println!("{}", msg);
                continue;
            }
            Err(_) => {
                // println!("Invalid");
                continue;
            }
        };
        
        match entry {
            ReplEntry::Define(name, expr) => {
                // Check for duplicate definition
                if defines.contains_key(&name) {
                    println!("Duplicate binding");
                    continue;
                }
                
                // Evaluate the expression
                let start = ops.offset();
                
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    compile_to_jit(&expr, &mut ops, &mut defines);
                })) {
                    Ok(_) => {}
                    Err(_) => {
                        // println!("Invalid");
                        continue;
                    }
                }
                
                dynasm!(ops ; .arch x64 ; ret);
                
                match ops.commit() {
                    Ok(_) => {}
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                }
                
                let reader = ops.reader();
                let buf = reader.lock();
                let jitted_fn: extern "C" fn() -> i32 = unsafe { mem::transmute(buf.ptr(start)) };
                let result = jitted_fn();
                
                // Store the result
                defines = defines.update(name, result);
                // Don't print anything for define
            }
            ReplEntry::Expr(expr) => {
                let start = ops.offset();
                
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    compile_to_jit(&expr, &mut ops, &mut defines);
                })) {
                    Ok(_) => {}
                    Err(_) => {
                        // println!("Invalid");
                        continue;
                    }
                }
                
                dynasm!(ops ; .arch x64 ; ret);
                
                match ops.commit() {
                    Ok(_) => {}
                    Err(_) => {
                        println!("Invalid");
                        continue;
                    }
                }
                
                let reader = ops.reader();
                let buf = reader.lock();
                let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
                let result = jitted_fn();
                
                print_result(result);
            }
        }
    }
    
    Ok(())
}
