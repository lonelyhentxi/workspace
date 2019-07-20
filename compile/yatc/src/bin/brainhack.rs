extern crate llvm_sys_wrapper;

use llvm_sys_wrapper::*;
use std::io;
use std::io::Read;

struct Compiler {
    ctx: Context,
    builder: Builder,
    module: Module,
    buffer: LLVMValueRef,
    ptr: LLVMValueRef,
    main_func: Function
}

impl Compiler {
    fn new(module_name: &str) -> Compiler {
        let context = Context::global_context();
        let builder = context.create_builder();
        let module = context.create_module(module_name);

        // setup main function
        let fun_type = fn_type!(context.VoidType());
        let main_function = module.add_function("main", fun_type);
        let entry_block = main_function.append_basic_block("entry");
        builder.position_at_end(entry_block);

        // setup calloc function
        let calloc_type = fn_type!(context.Int8PointerType(), context.Int64Type(), context.Int64Type());
        let calloc_func = module.get_or_add_function("calloc", calloc_type);

        // setup memory
        let data = builder.build_alloca(context.Int8PointerType());
        let ptr = builder.build_alloca(context.Int8PointerType());

        let mut args = [context.UInt64(30000), context.UInt64(1)];
        let data_ptr = builder.build_call(calloc_func.as_ref(), &mut args);

        builder.build_store(data_ptr, data);
        builder.build_store(data_ptr, ptr);

        Compiler {
            ctx: context,
            builder,
            module,
            buffer: data,
            ptr,
            main_func: main_function
        }
    }

    fn end_emit(&self){
        // setup free function
        let free_type = fn_type!(self.ctx.VoidType(), self.ctx.Int8PointerType());
        let free_func = self.module.get_or_add_function("free", free_type);

        // free memory
        let mut args = [self.builder.build_load(self.buffer)];
        self.builder.build_call(free_func.as_ref(), &mut args);

        // return void
        let _ret = self.builder.build_ret_void();
    }

    fn emit_move_ptr(&self, diff: i64){
        let mut indices = [self.ctx.SInt32(diff as u64)];
        let gep = self.builder.build_inbounds_gep(self.builder.build_load(self.ptr), &mut indices);
        self.builder.build_store(gep, self.ptr);
    }

    fn emit_add(&self, diff: i64){
        let tmp = self.builder.build_load(self.ptr);
        let add = self.builder.build_add(self.builder.build_load(tmp), self.ctx.SInt8(diff as u64));
        self.builder.build_store(add, tmp);
    }

    fn emit_put(&self){
        // setup putchar function
        let putchar_type = fn_type!(self.ctx.Int32Type(), self.ctx.Int32Type());
        let putchar_func = self.module.get_or_add_function("putchar", putchar_type);

        let val = self.builder.build_load( self.builder.build_load(self.ptr) );
        let ext_val = self.builder.build_sext(val, self.ctx.Int32Type());
        let mut args = [ext_val];
        let _call = self.builder.build_call(putchar_func.as_ref(), &mut args);
    }

    fn emit_while_start(&self, chars: &mut std::str::Chars){
        let cond_block = self.main_func.append_basic_block("while_cond");
        let body_block = self.main_func.append_basic_block("while_body");
        let end_block = self.main_func.append_basic_block("while_end");

        self.builder.build_br(cond_block);
        self.builder.position_at_end(cond_block);

        let load = self.builder.build_load( self.builder.build_load(self.ptr) );
        let cond = self.builder.build_icmp_ne(load, self.ctx.UInt8(0));
        self.builder.build_cond_br(cond, body_block, end_block);
        self.builder.position_at_end(body_block);

        self.compile(chars);

        self.builder.build_br(cond_block);
        self.builder.position_at_end(end_block);
    }

    fn dump(&self){
        match self.module.verify() {
            Ok(_) => self.module.dump(),
            Err(msg) => panic!("Error: {}", msg),
        }
    }

    #[allow(dead_code)]
    fn run(&self){
        match self.module.verify() {
            Ok(_) => {
                let interperter = self.module.create_interpreter().unwrap();
                let named_function = self.module.named_function("main");
                let mut params = [];
                let run_result = interperter.run_function(named_function.as_ref(), &mut params);
                let _ = run_result.to_int();
            },
            Err(msg) => panic!("Error: {}", msg)
        }
    }

    fn compile(&self, chars: &mut std::str::Chars){
        while let Some(ch) = chars.next() {
            match ch {
                '>' => self.emit_move_ptr(1),
                '<' => self.emit_move_ptr(-1),
                '+' => self.emit_add(1),
                '-' => self.emit_add(-1),
                '.' => self.emit_put(),
                '[' => self.emit_while_start(chars),
                ']' => return,
                _ => (),
            }
        }
    }
}

#[allow(unused_must_use)]
fn main() {
    // initialize LLVM
    LLVM::initialize();

    // create compiler
    let compiler = Compiler::new("brainhack");

    // read input
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer);

    // compile
    let mut chars = buffer.chars();
    compiler.compile(&mut chars);

    // end compile
    compiler.end_emit();

    // dump
    compiler.dump();
}