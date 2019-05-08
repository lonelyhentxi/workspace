use llvm_sys_wrapper::*;
use std::sync::Arc;

use ast::*;

pub struct Codegen {
    context: Context,
    builder: Builder
}

impl Codegen {
    pub fn new(module_name: &str) -> Codegen {
        let context = Context::global_context();
        let builder = context.create_builder();

        Codegen {
            context,
            builder,
        }
    }
}

pub type IRBuildingResult = Result<(),String>;

pub trait IRBuilder {
    fn build(&self, gen: &mut Codegen, module:& mut Module) -> IRBuildingResult;
}

impl<'a> IRBuilder for Chunk<'a> {
    fn build(&self, gen: &mut Codegen, module:& mut Module) -> IRBuildingResult {
        let mut res = vec![];
        for stat in &self.statements {
           stat.build(gen,module)?
        }
        Ok(())
    }
}

impl<'a> IRBuilder for Statement<'a> {
    fn build(&self, gen: &mut Codegen, module:& mut Module) -> IRBuildingResult {
        use Statement::*;
        match *self {
            Statement::Assignment(ref assignment) => assignment.build(assignment)?,

        }
    }
}