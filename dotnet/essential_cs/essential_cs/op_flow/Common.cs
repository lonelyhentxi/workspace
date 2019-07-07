using System;
using System.Collections.Generic;
using System.Text;

namespace essential_cs.op_flow
{
    class Common
    {
        public static void Main()
        {
            {
                // # 操作符和 Java 中类似，略
                // 浮点类型可能造成非预期的不相等
                // 非法的计算还可能出现 NaN
                // 不可以实现自定义 C# 操作符
                // 常量表达式和常量符号，const 略
            }
            {
                // # 控制流概述
                // if else while do-while for foreach continue switch break goto，略
            }
            // # 代码块，略
            // # 代码块、作用域和声明空间
            // C# 与 C++ 不同，对于声明局部变量的块，在声明引用属于非法
            {
                // # 布尔表达式
                // 大体通用，略
                {
                    // ## 空接合操作符
                    // 如果这个值为空，就使用另一个操作符
                    string a = null;
                    Console.WriteLine(a ?? "alter null");
                }
                {
                    // ## null 条件操作符
                    // xxx?.yyy.zzz
                    // 检查是否为空，不为空就短路表达式并返回 null
                    // 可以在方法和数组使用
                    // 数组调用时检查 null，但是越界仍会引发问题
                }
            }
            // # 按位操作符，略
            {
                // # C# 预处理指令
                // #if #elif #else #define #undef #error #warning #pragma #line #region
                // ## 包括和排除代码，略
                // ## 定义预处理符号，略
                // ## 生成错误和警告，略
                // ## 关闭警告消息
#pragma warning disable 1030
#pragma warning restore 1030
                // ## 指定行号
#line 100 "Common.cs"
                // ## 可视编辑器提示
                #region Display Common Board
                Console.WriteLine("Hello.");
                #endregion Display Common Board
            }
        }
    }
}