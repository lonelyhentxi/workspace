

fn main() {
    // FFI
    // rust 支持与 c 语言 abi 兼容
    // rust 和 c 语言可以互相调用
    {
        // 什么是 FFI
        // rust 不支持从源码级别的相互交互，而支持库级别的
        {
            //  --crate-type [bin|lib|rlib|dylib|cdylib|staticlib|proc-macro]
            // cdylib 和 staticlib 是 c 库
            // 指定目标文件类型有两种方式
            // 1. rustc --crate-type=staticlib test.rs
            // 2. 在源码入口指定 #![crate_type = "staticlib"]
        }
        // 要和目标 c 库采用相同的工具链
        {
            // 接口设计注意：
            // 暴露的函数使用 extern "C" 修饰
            // 使用#[no_mangle]修饰函数，避免名字重整
            // 函数参数、返回值中使用的类型，必须在 rust 和 C 中具有相同的内存布局
        }
    }
    {
        // 从 C 调用 rust 库
        // 见 src/ffi/clib.rs
        // 还可以使用 #[export_name = "my_whatever_name"] 来指定名字
    }
    {
        // 更复杂的数据类型
        // 对于简单的类型，直接使用标准库中定义好的 std::os::raw 里面的类型就够了
        // 复杂的类型需要我们手动封装，比如结构体就需要使用 #[repr(C)] 修饰，保证结构体在 Rust 和 C 内存布局一致
        //
    }
}