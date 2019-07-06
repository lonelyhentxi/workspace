fn main() {
    // 项目和模块
    // rust 用了两个概念来管理项目，一个是 crate，一个是 mod
    // crate 简单的理解是一个项目。 crate 是 rust 中的独立编译单元，每个 crate 对应生成一个库或可执行文件。
    // mod 简单的理解就是命名空间。 mod 可以嵌套，可以控制内部单元的可见性。
    // 重要区别：crate 之间不能出现循环引用，mod 可以
    {
        // cargo
        // cargo 是 rust 的包管理工具
        {
            // 新建项目
            // cargo new hello_world --bin # 新建可执行程序项目
            // cargo new hello_world --lib # 新建库项目
        }
        {
            // 构建项目
            // cargo build # 编译项目，默认生成 debug 版本
            // cargo build --release # 编译项目，生成 release 版本
        }
        {
            // 运行项目
            // cargo run # or --release 运行项目
            // 对应程序位于 /target/debug(release)/hello_world
        }
        {
            // 入口
            // 对于库项目，入口位于 src/lib.rs
            // 对于可执行项目，入口位于 src/lib.rs
        }
        {
            // 库的调用
            // 对于本地项目
            /*
            [dependencies]
            hello_world_lib = {path="./hello_world_lib"}
            */
            // 对于源中的项目
            /*
            [dependencies]
            lazy_static = "1.0.0"
            */
        }
        {
            // 其它常用命令
            // cargo build --verbose 查看详细的编译命令
            // (以下省略 cargo)
            // check 只执行语法检查，而不做代码优化以及生成可执行程序
            // clean 清理以前的编译结果
            // doc 生成该项目的文档
            // test 执行单元测试
            // bench 执行性能测试
            // update 升级所有依赖的版本
            // install 安装可执行程序
            // uninstall 删除可执行程序
            // tree 把所有依赖项的树形目录打印出来
        }
    }
    {
        // 项目依赖
        {
            {
                // 一个示例
                /*
                [dependencies]
                lazy_static = "1.0.0"
                rand = { git = "https://github.com/rust-lang-nursery/rand.git", branch = "master" }
                my_own_project = { path = "/my/local/path", version = "0.1.0" }
                */
            }
            {
                // 语义化版本号
                // 1.0.0 以前是不稳定版本，如果出现不兼容的改动，升级次版本号
                // 1.0.0 以后出现了 break changes，升级主版本号
                // 1.0.0 以后兼容性的增加公开 api，升级次版本号
            }
            {
                // 版本号的模糊匹配
                // ^1.2.3 [1.2.3,2.0.0)
                // ~1.2.3 [1.2.3,1.3.0)
                // 1.* [1.0.0,2.0.0]
                // 比较符号，一目了然
            }
            {
                // 来自 git 仓库的依赖
                // git 指定 repo
                // branch 指定 branch
                // commit 指定 commit
                // tag 指定 tag
            }
            {
                // 文件路径
                // 使用该路径下的 cargo.toml
                // 库尽量不要添加 cargo.lock，避免版本锁死
                // 可执行添加，保证在不同的机器上生成的是相同的二进制文件
            }
            {
                // 对于依赖项必须
                // 在 cargo.toml 和源文件的入口处添加说明
                // extern crate hello as hi;
            }
        }
        {
            // 配置
            // cargo 也支持配置文件，可以存在多份，并且有优先级关系
            // 可以放在当前文件夹的 .cargo/config 位置
            // 用户级别的默认配置，放在 $HOME/.cargo/config 位置
            {
                // 一份示例配置
                /*
                    [cargo-new]
                    // 可配置的默认名字和 email，会出现在新建项目的 Cargo.toml 中
                    name = "..."
                    email = "..."
                    [build]
                    jobs = 1 // 并行执行的 rustc 程序数量
                    rustcflags = ["...","..."] // 编译时传递给 rustc 的额外命令行参数
                    [term]
                    verbose = false // 执行命令时是否显示详细信息
                    color = 'auto' // 控制台内的彩色显示
                    [alias]
                    b = "build"
                */
            }
        }
        {
            // workspace
            {
                // 概念
                // 为了让不同的 crate 之间能够共享一些信息， cargo 提供了一个 workspace 概念
                // 一个 workspace 可以包含多个项目
                // 所有的项目共享一个 cargo.lock，依赖是同样的版本，输出的目标文件在同一个文件内
            }
            {
                // 使用
                // 把所有的项目放到同一个文件夹下
                // 设置一个公共的 Cargo.toml
                /*
                    [workspace]
                    members = [
                        "project1","lib1"
                    ]
                */
            }
        }
        {
            // build.rs
            {
                // cargo 还允许用户在正式编译开始前执行一些自定义的逻辑
                /*
                    [package]
                    build = "build.rs"
                */
                // 运行 cargo build 的时候，先把这个 build.rs 编译成可执行程序，然后运行这个程序
            }
            {
                // 一般用于：
                // 提前调用外部编译工具，比如调用 gcc 编译一个 c 库
                // 在操作系统中查找 c 库的位置
                // 根据某些配置，自动生成源码
                // 执行某些平台相关的配置
            }
            {
                // build.rs 中甚至可以在依赖其它的库
                // 在 [build-dependencies] 中指定
            }
            {
                // build.rs 可以读取一些 crate 的信息，通过预先设置好的环境变量
                // 例如
                {
                    // CARGO_MANIFEST_DIR
                    // 当前 crate 的 Cargo.toml 路径
                    // CARGO_PACKAGE_NAME
                    // OUT_DIR
                    // build.rs 输出路径
                    // HOST
                    // 当前 rustc 编译器的平台特性
                    // OPT_LEVEL
                    // 优化级别
                    // PROFILE
                    // debug 还是 release 版本
                }
            }
            {
                /*
                    引入指令
                    include!(concat!(env!("OUT_DIR"),"/commit_id.rs"));
                    include! 宏可以直接把目标文件中的内容在编译期复制到当前位置
                */
            }
        }
    }
    {
        // 模块管理
        // 每个 crate 会自动产生一个同名的根模块
        {
            //  在 crate 中创建新模块的方式
            // 在一个文件中创建内嵌模块，使用 mod 关键字
            // 独立的文件是模块，模块名为文件名
            // 一个文件夹也可以是模块，文件夹下要有 mod.rs
            // 在 mod.rs 中使用 re-export 可以大幅简化
            // 在入口处需要声明所有一级子模块，否则不会编译
        }
        {
            // 可见性
            {
                // 模块内部元素默认都是私有
                // pub 修饰的 trait 内部的关联元素，内部成员默认公开
                // pub enum 内部成员默认公开
                // 如果一个元素私有，那么只有本模块以及其子模块可以访问
                // 如果一个元素公开，上一级才有权使用
            }
            {
                // 使用诸如 pub use self::inner_mod1::method1; 的形式可以将模块 re-export
            }
            {
                // 为了更加清楚的限制可见性
                /*
                    pub(crate)
                    pub(in xxx_mod)
                    pub(self) 或者 pub(in self)
                    pub(super) 或者 pub(in super)
                */
            }
        }
        {
            // use 关键字
            {
                // 以 :: 开头的路径，代表全局路径，从 crate 的根部开始计算
                // 以 super 开始的路径，从上层开始计算
                // 以 self 开始的路径，从本层开始计算
            }
            {
                // 如果经常需要重复写很长的路径，可以使用 use 语句把对应元素引入到当前作用域
                // 使用大括号，可以引入多个元素
                // 大括号可以嵌套使用
                // * 可以引入所有元素
                // 允许使用 as 重命名
            }
        }
    }
}