#![allow(dead_code)]

fn main() {
    // 文档和测试
    {
        // 文档
        // rust 也支持使用注释来编写规范的文档
        // 可以使用 rustdoc 工具把源码中的文档提取出来，生成规范的 HTML 等格式
        // 在 cargo 中可以使用 cargo doc 命令生成文档
        {
            // 1. 行注释
            /* 2. 块注释 */
            // /// 特殊注释
            // //! 特殊注释
            // /** 特殊注释
            // */
            // /*! 特殊注释
            // */
            // 以上注释都会被视为文档
        }
        {
            mod foo {
                //! 这块文档是给 `foo` 模块做的说明

                /// 这块文档是给函数 `f` 做的说明
                fn f() {
                    // 这块注释不是文档的一部分
                }
            }
        }
        {
            // 文档内部支持 markdown
            // /// 文档中的代码要用 `code` 标注
            // /** 代码块要用 ```
            //        this is code block
            //   ```
            //    标注
            // */
            // */
            // 如果文档太长，也可以写在独立 markdown 文件中
            // #![feature(external_doc)]
            // #[doc(include = "external-doc.md")]
        }
    }
    {
        // 测试
        // rust 内置了一套单元测试工具，cargo test 即可运行
        {
            // 示例 1 见 doc_test 项目
        }
        {
            // #[cfg(test)] 的意思是只有 test 的时候才会生效
            // 还有更高级的用法，如：
            // #[cfg(all(unix, target_pointer_width = "32"))]
            // #[cfg(not(foo))]
            // #[cfg(any(not(unix),all(target_os="macos",target_arch = "powerpc")))]
        }
        {
            // 我们还可以自定义开关，见 Cargo.toml
            #[cfg(feature="my_feature_name")] {
                println!("my_feature_name enables!");
            }
            // 使用 cargo build --features "my_feature_name" 将属性传进去
        }
        {
            // 用户可以使用 #[ignore] 标记测试暂时忽略这个测试
            // 可以使用 #[bench] 添加性能测试
            // 需要添加
            // #![feature(test)]
            // extern crate test;
            // 使用 cargo bench 可以执行测试
        }
        {
            // assert_eq 检查相等
            // assert_ne 检查不等
            // #[should_panic] 检查应该崩溃的情况
        }
    }
}