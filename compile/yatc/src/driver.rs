use regex::Regex;
use std::io::Write;
use std::io;
use std::process::exit;
use std::iter::IntoIterator;

use crate::codegen;
use crate::codegen::{Codegen, IRBuilder, Module};
use crate::lexer::*;
use crate::parser::*;
use crate::util;
use crate::target::write_asm_code;
use llvm_sys_wrapper::{fn_type,LLVMFunctionType,LLVM};

pub use self::Stage::*;

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum Stage {
    AST,
    Tokens,
    Jit,
    Interpreter,
    Target,
}

lazy_static! {
    static ref EXEC_FUNC_REGEX: Regex = Regex::new(r"^$").unwrap(); // only for future plugin
}

pub fn main_loop(stage: Stage) {
    LLVM::initialize();
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();
    let mut parser_settings = default_parser_settings();
    let mut gen = codegen::Codegen::new();
    let mut ir_container = Module::new(&mut gen, "cli");

    let void_type = fn_type!(gen.context.VoidType());
    let mut engine_res: Result<llvm_sys_wrapper::Engine, String> = Err("empty engine".to_string());
    if  stage == Jit {
        engine_res = ir_container.create_jit_engine();
    }
    else if stage == Interpreter {
        engine_res = ir_container.create_interpreter();
    }
    let mut engine: Option<llvm_sys_wrapper::Engine> = None;
    if stage == Interpreter || stage==Jit {
        match engine_res {
            Err(err) => {
                print!("{}, will not exec.", err);
                stdout.flush().unwrap();
            }
            Ok(engine_content) => {
                engine = Some(engine_content);
            }
        }
    }

    let printf_type = fn_type!(gen.context.Int32Type(),gen.context.CharPointerType(),,,);
    let printf_func = ir_container.add_function("printf", printf_type);

    {
            let printlf_type = fn_type!(gen.context.DoubleType(),gen.context.DoubleType());
            let printlf_func = ir_container.add_function("show", printlf_type);
            let printlf_block = printlf_func.append_basic_block("entry");
            gen.builder.position_at_end(printlf_block);
            let fmt_d = gen.builder.build_global_string_ptr("%lf\n");
            let mut args = [fmt_d,printlf_func.get_param(0u32)];
            gen.builder.build_call(printf_func.as_ref(),&mut args);
            gen.builder.build_ret(printlf_func.get_param(0u32));
    }

    let mut all_tokens = vec![];
    let mut all_ast_nodes = vec![];
    let mut statements = vec![];


    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin
            .read_line(&mut input)
            .ok()
            .expect("Failed to read line");
        if input.as_str().starts_with(".quit") {
            break;
        }

        // the constructed AST
        let mut ast: Vec<ASTNode> = Vec::new();
        // tokens left from the previous lines
        let mut prev = Vec::new();
        loop {
            let tokens = tokenize(input.as_str(), 0);
            if stage == Tokens {
                println!("{:?}", &tokens);
                all_tokens.extend(tokens.into_iter());
                continue 'main;
            }
            prev.extend(tokens.into_iter().map(|item| item.kind));

            let parsing_result = parse(prev.as_slice(), ast.as_slice(), &mut parser_settings);
            match parsing_result {
                Ok((parsed_ast, rest)) => {
                    ast.extend(parsed_ast.into_iter());
                    if rest.is_empty() {
                        // we have parsed a full expression
                        break;
                    } else {
                        prev = rest;
                    }
                }
                Err(message) => {
                    println!("Error occured: {}", message);
                    continue 'main;
                }
            }
            print!(". ");
            stdout.flush().unwrap();
            input.clear();
            stdin
                .read_line(&mut input)
                .ok()
                .expect("Failed to read line");
        }

        if stage == AST {
            println!("{:?}", ast);
            all_ast_nodes.extend(ast.into_iter());
            continue;
        }
        for (i,ast_node) in ast.iter().enumerate() {
            match ast_node.codegen(&mut gen,&mut ir_container) {
                Ok(value) => {
                    Codegen::dump(value);
                    stdout.flush().unwrap();
                    if let FunctionNode(ref func) = &ast_node {
                        if EXEC_FUNC_REGEX.is_match(&func.prototype.name) {
                            if stage == Target {
                                statements.push(value);
                            }
                            else if let Some(ref engine_core) = &engine {
                                if stage==Jit {
                                    statements.push(value);
                                }
                                else if stage ==Interpreter {
                                    let mut empty_args = [];
                                    let void_type = fn_type!(gen.context.VoidType());
                                    let anoy_fun = ir_container.add_function("",void_type);
                                    let entry = anoy_fun.append_basic_block("enrty");
                                    gen.builder.position_at_end(entry);
                                    let ret = gen.builder.build_call(value,&mut empty_args);
                                    if i==ast.len()-1 {
                                        let mut printlf_args = [ret];
                                        gen.builder.build_call(ir_container.named_function("show").as_ref(), &mut printlf_args);
                                    }
                                    let mut anoy_args = [];
                                    gen.builder.build_ret_void();
                                    engine_core.run_function(anoy_fun.as_ref(),&mut anoy_args);
                                }
                            };
                        }
                    };
                },
                Err(message) => println!("Error occured: {}", message),
            }
        }
    }

    if stage == Tokens {
        util::write_json_to_file("assets/tokens.json", &all_tokens, "write tokens failed.");
        return;
    }
    if stage == AST {
        util::write_json_to_file("assets/asts.json", &all_ast_nodes, "write asts failed.");
        return;
    }
    if stage == Interpreter {
        ir_container.dump();
        ir_container.print_module_to_file("assets/interpreter.ll")
                .unwrap_or_else(|err| { writeln!(std::io::stderr(), "{}, write interpreter ir failed.",err).ok();
                        exit(-1);});
        return;
    }
    let main_func = ir_container.add_function("main", void_type);
    let main_block = main_func.append_basic_block("entry");
    gen.builder.position_at_end(main_block);
    for fun in &statements {
        let mut args = [];
        gen.builder.build_call(*fun, &mut args);
    }
    gen.builder.build_ret_void();
    ir_container.run_module_pass();
    ir_container.dump();
    ir_container.print_module_to_file("assets/compiler.ll")
    .unwrap_or_else(|err| 
    { writeln!(std::io::stderr(), "{}, write compiler ir failed.",err).ok();
                        exit(-1);});
    if stage == Jit {
        engine_res = ir_container.create_jit_engine();
        match engine_res {
            Err(err) => {
                print!("{}, will not exec.", err);
                stdout.flush().unwrap();
            }
            Ok(engine_core) => {
                let mut args = [];
                engine_core.run_function(main_func.as_ref(), &mut args);
            }
        }
        return;
    } 
    if stage == Target {
        write_asm_code(ir_container, "assets/target.s", "write asm failed.")
    }
}
