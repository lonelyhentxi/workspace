use std::collections::HashMap;
use std::ops::Deref;

use std::iter;

use llvm_sys;
use llvm_sys_wrapper::*;
use parser;

pub struct Codegen {
    context: Context,
    builder: Builder,
    named_values: HashMap<String, LLVMValueRef>,
    ty: LLVMTypeRef,
}

pub struct Module {
    module: llvm_sys_wrapper::Module,
    params_map: HashMap<String, HashMap<String, usize>>,
}

impl Module {
    pub fn new(module: llvm_sys_wrapper::Module) -> Module {
        Module {
            module,
            params_map: HashMap::new(),
        }
    }

    pub fn set_func_param(&mut self, function_name: &str, param_name: &str, idx: usize) {
        self.params_map
            .entry(function_name.to_string())
            .or_insert(HashMap::new())
            .insert(param_name.to_string(), idx);
    }

    pub fn get_func_param(&mut self, function_name: &str, param_name: &str) -> usize {
        *self
            .params_map
            .get(function_name)
            .unwrap()
            .get(param_name)
            .unwrap()
    }
}

impl Deref for Module {
    type Target = llvm_sys_wrapper::Module;
    fn deref(&self) -> &T {
        &self.module
    }
}

impl Codegen {
    pub fn new() -> Codegen {
        let context = Context::global_context();
        let builder = context.create_builder();
        let named_values = HashMap::new();
        let ty = context.DoubleType();

        Codegen {
            context,
            builder,
            named_values,
            ty,
        }
    }
}

pub type IRBuildingResult = Result<LLVMValueRef, String>;

fn error(message: &str) -> IRBuildingResult {
    Err(message.to_string())
}

pub trait IRBuilder {
    fn codegen(&self, context: &mut Codegen, module: &mut Module) -> IRBuildingResult;
}

impl IRBuilder for parser::ParsingResult {
    fn codegen(&self, context: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        match self {
            &Ok((ref ast, _)) => ast.codegen(context, module),
            &Err(ref message) => Err(message.clone()),
        }
    }
}

impl IRBuilder for Vec<parser::ASTNode> {
    fn codegen(&self, context: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        let mut result = error("empty AST");
        for node in self.iter() {
            result = Ok(node.codegen(context, module)?);
        }
        result
    }
}

impl IRBuilder for parser::ASTNode {
    fn codegen(&self, context: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        match self {
            &parser::ExternNode(ref prototype) => prototype.codegen(context, module),
            &parser::FunctionNode(ref function) => function.codegen(context, module),
        }
    }
}

impl IRBuilder for parser::Prototype {
    fn codegen(&self, gen: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        let fun_type = unsafe {
            let mut param_types = iter::repeat(gen.ty)
                .take(self.args.len())
                .collect::<Vec<_>>();
            llvm_sys::LLVMFunctionType(
                gen.ty,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0,
            )
        };
        let name = &self.name;
        let func = module.add_function(name, fun_type);
        for idx in 0..self.args.len() {
            module.set_func_param(name, &self.args[idx], idx);
        }
        Ok(func.as_ref())
    }
}

impl IRBuilder for parser::Function {
    fn codegen(&self, gen: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        gen.named_values.clear();

        let (fp, _) = self.prototype.codegen(gen, module)?;
        let mut function = Function::from_ptr(fp);

        let mut bb = function.append_basic_block("entry");
        gen.builder.position_at_end(bb);

        let prototype = &self.prototype;
        for i in 0..prototype.args.len() {
            let arg = &prototype.args[i];
            let arg_alloca = gen.builder.build_alloca_with_name(gen.ty, arg);
            gen.builder
                .build_store(function.get_param(i as u32), arg_alloca);
            gen.named_values.insert(arg.clone(), arg_alloca);
        }

        let body = match self.body.codegen(gen, module) {
            Ok(value) => value,
            Err(message) => {
                unsafe { LLVMDeleteFunction(function.to_ref()) };
                return Err(message);
            }
        };

        gen.builder.build_ret(body);
        gen.named_values.clear();
        Ok(function.to_ref())
    }
}

impl IRBuilder for parser::Expression {
    fn codegen(&self, gen: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        match self {
            parser::LiteralExpr(ref value) => Ok(gen.context.Double(*value)),

            parser::VariableExpr(ref name) => match gen.named_values.get(name) {
                Some(value) => {
                    let var = gen.builder.build_load(*value);
                    Ok(var)
                }
                None => error("unknown variable name"),
            },
            parser::UnaryExpr(ref operator, ref operand) => {
                let operand = operand.codegen(gen, module)?;
                let name = "unary".to_string() + operator;
                let function = module.named_function(name.as_str());
                let mut args_value = vec![operand];
                Ok(gen.builder.build_call(function.as_ref(), &mut args_value))
            }
            parser::BinaryExpr(ref name, ref lhs, ref rhs) => {
                if name.as_str() == "=" {
                    let var_name = match **lhs {
                        parser::VariableExpr(ref nm) => nm,
                        _ => return error("destination of '=' must be a variable"),
                    };

                    let value = rhs.codegen(context, module)?;

                    let variable = match gen.named_values.get(var_name) {
                        Some(vl) => *vl,
                        None => return error("unknown variable name"),
                    };
                    gen.builder.build_store(value, variable);
                    return Ok(value);
                }

                let lhs_value = lhs.codegen(context, module)?;
                let rhs_value = rhs.codegen(context, module)?;

                match name.as_str() {
                    "+" => Ok(gen
                        .builder
                        .build_add_with_name(lhs_value, rhs_value, "addtmp")),
                    "-" => Ok(gen
                        .builder
                        .build_sub_with_name(lhs_value, rhs_value, "subtmp")),
                    "*" => Ok(gen
                        .builder
                        .build_mul_with_name(lhs_value, rhs_value, "multmp")),
                    "<" => {
                        let cmp = gen
                            .builder
                            .build_fcmp_ult_with_name(lhs_value, rhs_value, "cmptmp");
                        let res = unsafe {
                            let val_name_ptr = CStringManager::new_cstring_as_ptr("booltmp");
                            llvm_sys::core::LLVMBuildUIToFP(
                                *gen.builder,
                                lhs_value,
                                gen.context.DoubleType(),
                                val_name_ptr,
                            )
                        };
                        Ok(res)
                    }
                    op => {
                        let name = "binary".to_string() + op;
                        let function = module.named_function(&name);
                        let mut args_value = vec![lhs_value, rhs_value];
                        Ok(gen.builder.build_call_with_name(
                            function.to_ref(),
                            &mut args_value,
                            "binop",
                        ))
                    }
                }
            }
            parser::CallExpr(ref name, ref args) => {
                let function = module.named_function(name);
                if function.params_count() as usize != args.len() {
                    return error("incorrect number of arguments passed");
                }
                let mut args_value = vec![];
                for arg in args.iter() {
                    let arg_value = arg.codegen(context, module)?;
                    args_value.push(arg_value);
                }

                Ok(gen
                    .builder
                    .build_call_with_name(function.to_ref(), &mut args_value, "calltmp"))
            }
            parser::ConditionalExpr {
                ref cond_expr,
                ref then_expr,
                ref else_expr,
            } => {
                let cond_value = cond_expr.codegen(context, module)?;
                let zero = gen.context.Double(0.);
                let ifcond = gen
                    .builder
                    .build_fcmp_one_with_name(cond_value, zero, "ifcond");

                let else0 = fib_func.append_basic_block("else0");
                let end = fib_func.append_basic_block("end");
                builder.build_cond_br(condition0, end, else0);
                builder.position_at_end(else0);
                let condition1 = builder.build_icmp_eq(arg, ctx.UInt64(1));
                let else1 = fib_func.append_basic_block("else1");
                builder.build_cond_br(condition1, end, else1);
                builder.position_at_end(else1);
                let sub2 = builder.build_sub(arg, ctx.UInt64(2));
                let mut args = [sub2];
                let fib_sub2 = builder.build_tail_call(fib_func.as_ref(), &mut args);
                let sub1 = builder.build_sub(arg, ctx.UInt64(1));
                let mut args = [sub1];
                let block = gen.builder.build_add();
                let mut function = block.get_parent();
                let mut then_block =
                    function.append_basic_block_in_context(&mut context.context, "then");
                let mut else_block =
                    function.append_basic_block_in_context(&mut context.context, "else");
                let mut merge_block =
                    function.append_basic_block_in_context(&mut context.context, "ifcont");
                context
                    .builder
                    .build_cond_br(ifcond, &then_block, &else_block);

                context.builder.position_at_end(&mut then_block);
                let (then_value, _) = try!(then_expr.codegen(context, module));
                context.builder.build_br(&merge_block);
                let then_end_block = context.builder.get_insert_block();

                context.builder.position_at_end(&mut else_block);
                let (else_value, _) = try!(else_expr.codegen(context, module));
                context.builder.build_br(&merge_block);
                let else_end_block = context.builder.get_insert_block();

                context.builder.position_at_end(&mut merge_block);

                let mut phi = unsafe {
                    PHINodeRef::from_ref(context.builder.build_phi(context.ty.to_ref(), "ifphi"))
                };
                phi.add_incoming(
                    vec![then_value].as_mut_slice(),
                    vec![then_end_block].as_mut_slice(),
                );
                phi.add_incoming(
                    vec![else_value].as_mut_slice(),
                    vec![else_end_block].as_mut_slice(),
                );

                Ok((phi.to_ref(), false))
            }

            &parser::LoopExpr {
                ref var_name,
                ref start_expr,
                ref end_expr,
                ref step_expr,
                ref body_expr,
            } => {
                let (start_value, _) = try!(start_expr.codegen(context, module));

                let preheader_block = context.builder.get_insert_block();
                let mut function = preheader_block.get_parent();

                let variable = create_entry_block_alloca(context, &function, var_name);
                context.builder.build_store(start_value, variable);

                let mut preloop_block =
                    function.append_basic_block_in_context(&mut context.context, "preloop");
                context.builder.build_br(&preloop_block);
                context.builder.position_at_end(&mut preloop_block);

                let old_value = context.named_values.remove(var_name);
                context
                    .named_values
                    .insert(var_name.clone(), variable.to_ref());

                let (end_value, _) = try!(end_expr.codegen(context, module));
                let zero = RealConstRef::get(&context.ty, 0.0);
                let end_cond =
                    context
                        .builder
                        .build_fcmp(LLVMRealONE, end_value, zero.to_ref(), "loopcond");

                let mut after_block =
                    function.append_basic_block_in_context(&mut context.context, "afterloop");
                let mut loop_block =
                    function.append_basic_block_in_context(&mut context.context, "loop");

                context
                    .builder
                    .build_cond_br(end_cond, &loop_block, &after_block);

                context.builder.position_at_end(&mut loop_block);
                try!(body_expr.codegen(context, module));

                let (step_value, _) = try!(step_expr.codegen(context, module));

                let cur_value = context.builder.build_load(variable, var_name);
                let next_value = context.builder.build_fadd(cur_value, step_value, "nextvar");
                context.builder.build_store(next_value, variable);

                context.builder.build_br(&preloop_block);

                context.builder.position_at_end(&mut after_block);

                context.named_values.remove(var_name);
                match old_value {
                    Some(value) => {
                        context.named_values.insert(var_name.clone(), value);
                    }
                    None => (),
                };

                Ok((zero.to_ref(), false))
            }

            &parser::VarExpr {
                ref vars,
                ref body_expr,
            } => {
                let mut old_bindings = Vec::new();
                let function = context.builder.get_insert_block().get_parent();
                for var in vars.iter() {
                    let (ref name, ref init_expr) = *var;
                    let (init_value, _) = try!(init_expr.codegen(context, module));
                    let variable = create_entry_block_alloca(context, &function, name);
                    context.builder.build_store(init_value, variable);
                    old_bindings.push(context.named_values.remove(name));
                    context.named_values.insert(name.clone(), variable);
                }

                let (body_value, _) = try!(body_expr.codegen(context, module));

                let mut old_iter = old_bindings.iter();
                for var in vars.iter() {
                    let (ref name, _) = *var;
                    context.named_values.remove(name);

                    match old_iter.next() {
                        Some(&Some(value)) => {
                            context.named_values.insert(name.clone(), value);
                        }
                        _ => (),
                    };
                }

                Ok((body_value, false))
            }
        }
    }
}
