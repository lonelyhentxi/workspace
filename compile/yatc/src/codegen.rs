use libc::c_char;
use std::collections::HashMap;
use std::ffi::CString;
use std::iter;
use std::ops::Deref;

use cstring_manager::CStringManager;
use llvm_sys;
use llvm_sys_wrapper::*;
use llvm_sys::transforms::ipo::*;
use llvm_sys::transforms::vectorize::*;
use parser;

pub struct Module {
    module: llvm_sys_wrapper::Module,
    function_passmanager: LLVMPassManagerRef,
    module_passmanager: LLVMPassManagerRef,
}

impl Module {
    pub fn new(gen: &mut Codegen, name: &str) -> Module {
        let module = gen.context.create_module(name);
        let module_passmanager = unsafe {
            use llvm_sys::core::*;
            use llvm_sys::transforms::scalar::*;
            let passmanager = LLVMCreatePassManager();
            LLVMAddSimplifyLibCallsPass(passmanager);
            LLVMAddFunctionInliningPass(passmanager);
            LLVMAddBasicAliasAnalysisPass(passmanager);
            LLVMAddTailCallEliminationPass(passmanager);
            LLVMAddLICMPass(passmanager);
            LLVMAddLoopDeletionPass(passmanager);
            LLVMAddLoopVectorizePass(passmanager);
            LLVMAddLoopIdiomPass(passmanager);
            LLVMAddPartiallyInlineLibCallsPass(passmanager);
            LLVMAddInstructionCombiningPass(passmanager);
            LLVMAddReassociatePass(passmanager);
            LLVMAddGVNPass(passmanager);
            LLVMAddCFGSimplificationPass(passmanager);
            LLVMAddGlobalDCEPass(passmanager);
            LLVMAddGlobalOptimizerPass(passmanager);
            LLVMInitializeFunctionPassManager(passmanager);
            passmanager
        };
        let function_passmanager = unsafe {
            use llvm_sys::core::*;
            use llvm_sys::transforms::scalar::*;
            let function_passmanager = LLVMCreateFunctionPassManagerForModule(module.as_ref());
            LLVMAddSimplifyLibCallsPass(function_passmanager);
            LLVMAddTailCallEliminationPass(function_passmanager);
            LLVMAddLICMPass(function_passmanager);
            LLVMAddLoopDeletionPass(function_passmanager);
            LLVMAddLoopVectorizePass(function_passmanager);
            LLVMAddLoopIdiomPass(function_passmanager);
            LLVMAddPartiallyInlineLibCallsPass(function_passmanager);
            LLVMAddInstructionCombiningPass(function_passmanager);
            LLVMAddReassociatePass(function_passmanager);
            LLVMAddGVNPass(function_passmanager);
            LLVMAddCFGSimplificationPass(function_passmanager);
            LLVMInitializeFunctionPassManager(function_passmanager);
            function_passmanager
        };
        Module { module, function_passmanager,module_passmanager }
    }



    pub fn get_function(&self, name: &str) -> Option<(LLVMValueRef, bool)> {
        let func_name_ptr = CString::new(name).unwrap();
        let named_function = unsafe {
            LLVMGetNamedFunction(
                self.module.as_ref(),
                func_name_ptr.as_ptr() as *const c_char,
            )
        };
        if named_function.is_null() {
            None
        } else {
            let defined = unsafe { llvm_sys::core::LLVMCountBasicBlocks(named_function) > 0 };
            Some((named_function, defined))
        }
    }

    pub fn run_function_pass(&self, function: LLVMValueRef) -> bool {
        unsafe {
            LLVMRunFunctionPassManager(self.function_passmanager, function) > 0
        }
    }

    pub fn run_module_pass(&self) -> bool {
        unsafe {
            LLVMRunPassManager(self.module_passmanager, self.module.as_ref()) > 0
        }
    }
}

impl Deref for Module {
    type Target = llvm_sys_wrapper::Module;
    fn deref(&self) -> &Self::Target {
        &self.module
    }
}
pub struct Codegen {
    pub context: Context,
    pub builder: Builder,
    pub named_values: HashMap<String, LLVMValueRef>,
    pub ty: LLVMTypeRef,
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

    pub fn get_insert_block(&self) -> LLVMBasicBlockRef {
        unsafe { llvm_sys::core::LLVMGetInsertBlock(self.builder.as_ref()) }
    }

    pub fn get_current_function(&self) -> LLVMValueRef {
        let block = self.get_insert_block();
        unsafe { llvm_sys::core::LLVMGetBasicBlockParent(block) }
    }

    pub fn append_basic_block_in_context(
        &mut self,
        function: LLVMValueRef,
        name: &str,
    ) -> LLVMBasicBlockRef {
        unsafe {
            let label_name_ptr = CStringManager::new_cstring_as_ptr(name);
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context.as_ref(),
                function,
                label_name_ptr,
            )
        }
    }

    pub fn set_func_param(function: LLVMValueRef, param_name: &str, idx: usize) {
        let param_name = CString::new(param_name).unwrap();
        let param = unsafe { llvm_sys::core::LLVMGetParam(function, idx as u32) };
        unsafe {
            llvm_sys::core::LLVMSetValueName(param, param_name.as_ptr() as *const i8);
        }
    }

    pub fn get_func_param(function: LLVMValueRef, idx: usize) -> LLVMValueRef {
        unsafe { llvm_sys::core::LLVMGetParam(function, idx as u32) }
    }

    pub fn count_func_param(function: LLVMValueRef) -> usize {
        unsafe { llvm_sys::core::LLVMCountParams(function) as usize }
    }

    pub fn dump(value: LLVMValueRef) {
        unsafe { llvm_sys::core::LLVMDumpValue(value) }
    }

    pub fn get_entry(function: LLVMValueRef) -> LLVMBasicBlockRef {
        unsafe {
            LLVMGetEntryBasicBlock(function)
        }
    }

    pub fn get_first_instruction(basic_block: LLVMBasicBlockRef) -> LLVMValueRef {
        unsafe {
            LLVMGetFirstInstruction(basic_block)
        }
    }

    pub fn position(builder: LLVMBuilderRef,basic_block:LLVMBasicBlockRef,instruction:LLVMValueRef) {
        unsafe {
            LLVMPositionBuilder(builder,basic_block, instruction)
        }
    }

    pub fn create_entry_block_alloca(&self,function: LLVMValueRef,var_name: &str) -> LLVMValueRef {
        let temp_builder = self.context.create_builder();
        let bb = Codegen::get_entry(function);
        let fi = Codegen::get_first_instruction(bb);
        Codegen::position(temp_builder.as_ref(), bb, fi);
        temp_builder.build_alloca_with_name(self.ty, var_name)
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
            Ok((ref ast, _)) => ast.codegen(context, module),
            Err(ref message) => Err(message.clone()),
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
    fn codegen(&self, gen: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        match self {
            parser::ExternNode(ref prototype) => prototype.codegen(gen, module),
            parser::FunctionNode(ref function) => function.codegen(gen, module),
        }
    }
}

impl IRBuilder for parser::Prototype {
    fn codegen(&self, gen: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        let fun = match module.get_function(&self.name) {
            Some((prev_definition, _)) => {
                if Codegen::count_func_param(prev_definition) != self.args.len() {
                    return error("declaration of function with different number of args");
                }
                prev_definition
            }
            None => {
                let mut param_types = iter::repeat(gen.ty)
                    .take(self.args.len())
                    .collect::<Vec<_>>();
                let fun_type = unsafe {
                    llvm_sys::core::LLVMFunctionType(
                        gen.ty,
                        param_types.as_mut_ptr(),
                        param_types.len() as u32,
                        0,
                    )
                };
                let func = module.add_function(&self.name, fun_type);
                for idx in 0..self.args.len() {
                    Codegen::set_func_param(func.as_ref(), &self.args[idx], idx);
                }
                func.as_ref()
            }
        };
        Ok(fun)
    }
}

impl IRBuilder for parser::Function {
    fn codegen(&self, gen: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        gen.named_values.clear();

        let function = self.prototype.codegen(gen, module)?;
        let fun_res = &module.get_function(&self.prototype.name);
        if fun_res.is_some() && fun_res.unwrap().1 {
            return error("redefiniation of function");
        }
        let bb = gen.append_basic_block_in_context(function, "entry");
        gen.builder.position_at_end(bb);

        let prototype = &self.prototype;
        for idx in 0..prototype.args.len() {
            let arg = &prototype.args[idx];
            let param = Codegen::get_func_param(function, idx);
            let arg_alloca = gen.create_entry_block_alloca(function, arg);
            gen.builder.build_store(param,arg_alloca);
            gen.named_values.insert(arg.clone(), arg_alloca);
        }

        let body = match self.body.codegen(gen, module) {
            Ok(value) => value,
            Err(message) => {
                unsafe { LLVMDeleteFunction(function) };
                return Err(message);
            }
        };

        gen.builder.build_ret(body);
        unsafe {
            LLVMDumpValue(function);
        }
        module.run_function_pass(function);
        println!("{:?}",gen.named_values);
        gen.named_values.clear();
        Ok(function)
    }
}

impl IRBuilder for parser::Expression {
    fn codegen(&self, gen: &mut Codegen, module: &mut Module) -> IRBuildingResult {
        match self {
            parser::LiteralExpr(ref value) => Ok(gen.context.Double(*value)),
            parser::VariableExpr(ref name) => match gen.named_values.get(name) {
                Some(value) => {
                    let var = gen.builder.build_load_with_name(*value, name);
                    Ok(var)
                }
                None => error("unknown variable name"),
            },
            parser::UnaryExpr(ref operator, ref operand) => {
                let operand = operand.codegen(gen, module)?;
                let name = "unary".to_string() + operator;
                let function = module.named_function(name.as_str());
                let mut args_value = vec![operand];
                Ok(gen.builder.build_call_with_name(function.as_ref(), &mut args_value,"unop"))
            }
            parser::BinaryExpr(ref name, ref lhs, ref rhs) => {

                if name.as_str() == "=" {
                    let var_name = match **lhs {
                        parser::VariableExpr(ref nm) => nm,
                        _ => return error("destination of '=' must be a variable")
                    };

                    let value = rhs.codegen(gen, module)?;

                    let variable = match gen.named_values.get(var_name) {
                        Some(vl) => *vl,
                        None => return error("unknown variable name")
                    };

                    gen.builder.build_store(value, variable);

                    return Ok(value)
                }
                let lhs_value = lhs.codegen(gen, module)?;
                let rhs_value = rhs.codegen(gen, module)?;

                match name.as_str() {
                    "+" => 
                        Ok(unsafe {
                            let name = CStringManager::new_cstring_as_ptr("addtmp");
                          llvm_sys::core::LLVMBuildFAdd(gen.builder.as_ref(),lhs_value,rhs_value, name)
                        }),
                    "-" => Ok(unsafe {
                            let name = CStringManager::new_cstring_as_ptr("subtmp");
                          llvm_sys::core::LLVMBuildFSub(gen.builder.as_ref(),lhs_value,rhs_value, name)
                        }),
                    "*" => Ok(unsafe {
                            let name = CStringManager::new_cstring_as_ptr("multtmp");
                          llvm_sys::core::LLVMBuildFMul(gen.builder.as_ref(),lhs_value,rhs_value, name)
                        }),
                    "<" => {
                        let cmp = gen
                            .builder
                            .build_fcmp_ult_with_name(lhs_value, rhs_value, "cmptmp");
                        let res = unsafe {
                            let val_name_ptr = CStringManager::new_cstring_as_ptr("booltmp");
                            llvm_sys::core::LLVMBuildUIToFP(
                                gen.builder.as_ref(),
                                cmp,
                                gen.context.DoubleType(),
                                val_name_ptr,
                            )
                        };
                        Ok(res)
                    }
                    op => {
                        let name = "binary".to_string() + op;
                         let function = match module.get_function(&name) {
                            Some((function,_)) => function,
                            None => return error("binary operator not found")
                        };
                        let mut args_value = vec![lhs_value, rhs_value];
                        Ok(gen.builder.build_call_with_name(
                            function,
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
                if module.get_function(name).is_none() {
                    return error("call function which have no definiation");
                }
                let mut args_value = vec![];
                for arg in args.iter() {
                    let arg_value = arg.codegen(gen, module)?;
                    args_value.push(arg_value);
                }
                Ok(gen
                    .builder
                    .build_call_with_name(function.as_ref(), &mut args_value, "calltmp"))
            }
            parser::ConditionalExpr {
                ref cond_expr,
                ref then_expr,
                ref else_expr,
            } => {
                let cond_value = cond_expr.codegen(gen, module)?;
                let zero = gen.context.Double(0.);
                let ifcond = gen
                    .builder
                    .build_fcmp_one_with_name(cond_value, zero, "ifcond");

                let function = gen.get_current_function();
                let then_block = gen.append_basic_block_in_context(function, "then");
                let else_block = gen.append_basic_block_in_context(function, "else");
                let merge_block = gen.append_basic_block_in_context(function, "ifcont");
                gen.builder.build_cond_br(ifcond, then_block, else_block);

                gen.builder.position_at_end(then_block);
                let then_value = then_expr.codegen(gen, module)?;
                gen.builder.build_br(merge_block);
                let then_end_block = gen.get_insert_block();

                gen.builder.position_at_end(else_block);
                let else_value = else_expr.codegen(gen, module)?;
                gen.builder.build_br(merge_block);
                let else_end_block = gen.get_insert_block();

                gen.builder.position_at_end(merge_block);
                let phi = gen.builder.build_phi_with_name(gen.ty, "ifphi");
                phi.add_incoming(then_value, then_end_block);
                phi.add_incoming(else_value, else_end_block);
                Ok(phi.as_ref())
            }

            parser::LoopExpr {
                ref var_name,
                ref start_expr,
                ref end_expr,
                ref step_expr,
                ref body_expr,
            } => {
                let start_value = start_expr.codegen(gen, module)?;
                let function = gen.get_current_function();

                let variable = gen.create_entry_block_alloca(function,var_name);
                gen.builder.build_store(start_value,variable);

                let preloop_block = gen.append_basic_block_in_context(function, "preloop");
                gen.builder.build_br(preloop_block);
                gen.builder.position_at_end(preloop_block);

                let old_value = gen.named_values.remove(var_name);
                gen.named_values.insert(var_name.clone(), variable);

                let end_value = end_expr.codegen(gen, module)?;
                let zero = gen.context.Double(0f64);
                let end_cond = gen
                    .builder
                    .build_fcmp_one_with_name(end_value, zero, "loopcond");

                let after_block = gen.append_basic_block_in_context(function, "afterloop");
                let loop_block = gen.append_basic_block_in_context(function, "loop");

                gen.builder.build_cond_br(end_cond, loop_block, after_block);

                gen.builder.position_at_end(loop_block);
                body_expr.codegen(gen, module)?;

                let step_value = step_expr.codegen(gen, module)?;
                let cur_value = gen.builder.build_load_with_name(variable,var_name);
                let next_value = unsafe {
                    let next_var_name = CStringManager::new_cstring_as_ptr("nextvar");
                    llvm_sys::core::LLVMBuildFAdd(gen.builder.as_ref(),cur_value, step_value, next_var_name)
                };
                gen.builder.build_store(next_value, variable);

                gen.builder.build_br(preloop_block);

                gen.builder.position_at_end(after_block);

                gen.named_values.remove(var_name);

                if let Some(value) = old_value {
                    gen.named_values.insert(var_name.clone(), value);
                };

                Ok(zero)
            }
            parser::VarExpr{ref vars, ref body_expr} => {
                let mut old_bindings = vec![];
                let function = gen.get_current_function();
                for var in vars.iter() {
                    let (ref name, ref init_expr) = *var;
                    let init_value = init_expr.codegen(gen, module)?;
                    let variable = gen.create_entry_block_alloca(function, name);
                    gen.builder.build_store(init_value, variable);
                    old_bindings.push(gen.named_values.remove(name));
                    gen.named_values.insert(name.clone(), variable);
                }
                println!("{:?}",gen.named_values);
                let body_value = body_expr.codegen(gen, module)?;

                let mut old_iter = old_bindings.iter();
                for var in vars.iter() {
                    let (ref name, _) = *var;
                    gen.named_values.remove(name);

                    if let Some(Some(value)) = old_iter.next()
                     {gen.named_values.insert(name.clone(), *value);
                    };
                }
                Ok(body_value)
            }

        }
    }
}
