#![feature(prelude_import)]
#![no_std]

#[prelude_import]
use ::std::prelude::v1::*;

#[macro_use]
extern crate std;

fn main() {
    {
        // introduction
        // rust 的宏是卫生宏，调用语法和函数明显不同
        // 宏的内部和外部有严格的访问空间界限
        // c/c++ 的宏在预处理阶段起作用，rust 的宏在语法解析阶段起作用，因此可以获得更多的上下文信息
        {
            // 实现编译期检查
            // println!("number1 {} number2 {}"); // 需要提供足够的参数，这是在编译期进行的检查
        }
        {
            // 实现编译期计算
            // 实现自动代码生成
            // 实现语法拓展
            // 泛型示例
            // 自定义宏只能通过 macro_rules! 宏或者编译期拓展实现，后者现在还不稳定
            {
                ::io::_print(::std::fmt::Arguments::new_v1_formatted(&["file ",
                    " line ",
                    "\n"],
                                                                     &match (&"src\\bin\\lib.rs",
                                                                             &13u32)
                                                                         {
                                                                             (arg0,
                                                                                 arg1)
                                                                             =>
                                                                                 [::std::fmt::ArgumentV1::new(arg0,
                                                                                                              ::std::fmt::Display::fmt),
                                                                                     ::std::fmt::ArgumentV1::new(arg1,
                                                                                                                 ::std::fmt::Display::fmt)],
                                                                         },
                                                                     &[::std::fmt::rt::v1::Argument {
                                                                         position:
                                                                         ::std::fmt::rt::v1::Position::At(0usize),
                                                                         format:
                                                                         ::std::fmt::rt::v1::FormatSpec {
                                                                             fill:
                                                                             ' ',
                                                                             align:
                                                                             ::std::fmt::rt::v1::Alignment::Unknown,
                                                                             flags:
                                                                             0u32,
                                                                             precision:
                                                                             ::std::fmt::rt::v1::Count::Implied,
                                                                             width:
                                                                             ::std::fmt::rt::v1::Count::Implied,
                                                                         },
                                                                     },
                                                                         ::std::fmt::rt::v1::Argument {
                                                                             position:
                                                                             ::std::fmt::rt::v1::Position::At(1usize),
                                                                             format:
                                                                             ::std::fmt::rt::v1::FormatSpec {
                                                                                 fill:
                                                                                 ' ',
                                                                                 align:
                                                                                 ::std::fmt::rt::v1::Alignment::Unknown,
                                                                                 flags:
                                                                                 0u32,
                                                                                 precision:
                                                                                 ::std::fmt::rt::v1::Count::Implied,
                                                                                 width:
                                                                                 ::std::fmt::rt::v1::Count::Implied,
                                                                             },
                                                                         }]));
            };
        }
        {}
        {
            #[allow(unused_variables)]
            let v = <[_]>::into_vec(box [1, 2, 3, 4, 5]);
        }
    }
    {
        macro_rules! hashmap(( $ ( $ key : expr => $ val : expr ) , * ) => {
                             {
                             let mut map = :: std :: collections :: HashMap ::
                             new (  ) ; $ ( map . insert ( $ key , $ val ) ; )
                             * map } });
        let counts =
            {
                let mut map = ::std::collections::HashMap::new();
                map.insert('A', 0);
                map.insert('C', 0);
                map.insert('G', 0);
                map.insert('T', 0);
                map
            };
        {
            ::io::_print(::std::fmt::Arguments::new_v1_formatted(&["", "\n"],
                                                                 &match (&counts, )
                                                                     {
                                                                         (arg0, )
                                                                         =>
                                                                             [::std::fmt::ArgumentV1::new(arg0,
                                                                                                          ::std::fmt::Debug::fmt)],
                                                                     },
                                                                 &[::std::fmt::rt::v1::Argument {
                                                                     position:
                                                                     ::std::fmt::rt::v1::Position::At(0usize),
                                                                     format:
                                                                     ::std::fmt::rt::v1::FormatSpec {
                                                                         fill:
                                                                         ' ',
                                                                         align:
                                                                         ::std::fmt::rt::v1::Alignment::Unknown,
                                                                         flags:
                                                                         0u32,
                                                                         precision:
                                                                         ::std::fmt::rt::v1::Count::Implied,
                                                                         width:
                                                                         ::std::fmt::rt::v1::Count::Implied,
                                                                     },
                                                                 }]));
        };
    }
}
