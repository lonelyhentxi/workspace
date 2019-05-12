use crate::codegen::Module;
use std::process::exit;
use std::ffi::{CString,CStr};
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::fs::OpenOptions;
use std::io::Write;
use std::{mem,ptr,slice};

fn membuf_as_slice(mbuf: & LLVMMemoryBufferRef) -> &[u8] {
    unsafe {
        let p = LLVMGetBufferStart(*mbuf);
        let len = LLVMGetBufferSize(*mbuf);
        slice::from_raw_parts(p as *const _, len as usize)
    }
}

pub fn write_asm_code(module: Module,filename: &str,err_prompt: &str) {
    let ty = llvm_sys::target_machine::LLVMCodeGenFileType::LLVMAssemblyFile;
    let llmod = (*module).as_ref();
    use llvm_sys::target_machine::*;
    use llvm_sys::target_machine::LLVMCodeGenOptLevel::*;
    use llvm_sys::target_machine::LLVMRelocMode::*;
    use llvm_sys::target_machine::LLVMCodeModel::*;
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(filename)
      .unwrap_or_else(|err| { writeln!(std::io::stderr(), "{},{}",err, err_prompt).ok(); exit(-1);});
    let triple_cstring: &CStr = &unsafe { CString::from_raw(llvm_sys::target_machine::LLVMGetDefaultTargetTriple()) };
    let triple = triple_cstring.as_ptr();
    unsafe {
        let mut target = mem::uninitialized();
        LLVMGetTargetFromTriple(triple, &mut target, ptr::null_mut());
        let tm = LLVMCreateTargetMachine(target,
                                         triple,
                                         b"\0".as_ptr() as *const _,
                                         b"\0".as_ptr() as *const _,
                                         LLVMCodeGenLevelAggressive,
                                         LLVMRelocDefault,
                                         LLVMCodeModelDefault);

        let mut mbuf: LLVMMemoryBufferRef = mem::uninitialized();
        LLVMTargetMachineEmitToMemoryBuffer(tm, llmod, ty, ptr::null_mut(), &mut mbuf);
        let res =file.write_all(membuf_as_slice(&mbuf));

        LLVMDisposeMemoryBuffer(mbuf);
        LLVMDisposeTargetMachine(tm);
        res.unwrap_or_else(|err| { writeln!(std::io::stderr(), "{},{}",err, err_prompt).ok(); exit(-1);});
    }
}