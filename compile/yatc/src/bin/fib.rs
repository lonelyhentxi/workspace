extern crate llvm_sys_wrapper;

use llvm_sys_wrapper::*;

fn main() {
    LLVM::initialize();
    let ctx = Context::global_context();
    let builder = ctx.create_builder();
    let module = ctx.create_module("fib_example");

    let printf_type = fn_type!(ctx.Int32Type(),ctx.CharPointerType(),,,);
    let printf_func = module.add_function("printf", printf_type);
    let fun_type = fn_type!(ctx.Int64Type(),ctx.Int64Type());
    let fib_func = module.add_function("fib", fun_type);
    let entry_block = fib_func.append_basic_block("entry");
    builder.position_at_end(entry_block);
    let arg = fib_func.get_param(0);
    let condition0 = builder.build_icmp_eq(arg,ctx.UInt64(0));
    let else0 = fib_func.append_basic_block("else0");
    let end = fib_func.append_basic_block("end");
    builder.build_cond_br(condition0, end , else0);
    builder.position_at_end(else0);
    let condition1 = builder.build_icmp_eq(arg, ctx.UInt64(1));
    let else1 = fib_func.append_basic_block("else1");
    builder.build_cond_br(condition1, end, else1);
    builder.position_at_end(else1);
    let sub2  =  builder.build_sub(arg, ctx.UInt64(2));
    let mut args = [sub2];
    let fib_sub2 = builder.build_tail_call(fib_func.as_ref(), &mut args);
    let sub1 = builder.build_sub(arg, ctx.UInt64(1));
    let mut args = [sub1];
    let fib_sub1 = builder.build_tail_call(fib_func.as_ref(), &mut args);
    let sum = builder.build_add(fib_sub2,fib_sub1);
    builder.build_br(end);
    builder.position_at_end(end);
    let phi = builder.build_phi(ctx.Int64Type());
    phi.add_incoming(ctx.UInt64(0), entry_block);
    phi.add_incoming(ctx.UInt64(1), else0);
    phi.add_incoming(sum,else1);
    builder.build_ret(phi.as_ref());
    let fun_type = fn_type!(ctx.VoidType());
    let main_func = module.add_function("main", fun_type);
    let entry_block = main_func.append_basic_block("entry");
    builder.position_at_end(entry_block);
    let mut args = [ctx.UInt64(10)];
    let ret = builder.build_call(fib_func.as_ref(), &mut args);
    let fmt_d = builder.build_global_string_ptr("%lu\n");
    let mut args = [fmt_d,ret];
    builder.build_call(printf_func.as_ref(),&mut args);
    builder.build_ret_void();
    module.verify().unwrap();
    module.print_module_to_file("assets/fib.ll").unwrap(); 
    let engine = module.create_jit_engine().unwrap();
    let named_function = module.named_function("main");
    let mut params = [];
    let run_result = engine.run_function(named_function.as_ref(), &mut params);
    let _ = run_result.to_int();
    
}