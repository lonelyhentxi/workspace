using System;

namespace essential_cs.intro
{
    class Common
    {
        public static void Main(string[] args)
        {
            {
                // # HelloWorld 见 HelloWorld.cs
                // 给环境变量引入 csc
                // csc HelloWorld.cs
                // HelloWorld.exe or ./HelloWorld
            }
            {
                // # C# 语法基础
                // ## 关键字，略
                {
                    // ## 标识符
                    // C# 中流行 PascalCase 风格的大小写方式
                    // 有的标识符， 例如 Main 在 C# 中有特殊的含义
                    // 不符合规范推荐的命名方式也有使用
                }
                // ## 类型定义，略
                {
                    // ## Main
                    // Main 可以为 void 或者 int 等
                    // args 只带有无程序名参数
                    foreach (var s in args)
                    {
                        System.Console.Write(s + " ");
                    }

                    System.Console.WriteLine();
                    // System.Environment.CommandLine 含有所有全部参数的字符串
                    foreach (var s in System.Environment.CommandLine.Split(' '))
                    {
                        System.Console.Write(s + " ");
                    }

                    System.Console.WriteLine();
                    // c++/java 全部是小写的
                    // 要指定 static
                }
                // ## 语句和语句分隔符，略
                // ## 空白，略
                // ## 使用变量，略
                // ## 数据类型，略
                // ## 变量的声明，略
                // ## 变量的赋值，略
                // ## 变量的使用，略
            }
            {
                // # 控制台输入和输出
                {
                    // ## 从控制台获得输入
                    Console.WriteLine("Input your string:");
                    var anyInput = System.Console.ReadLine();
                    Console.WriteLine("your input is: " + anyInput);
                    var readValue = Console.Read(); // 读取的是与对应的字符值对应的整数，如果没有更多的字符可用，返回 -1
                    if (readValue == -1)
                    {
                        Console.WriteLine("Invalid value.");
                    }
                    else
                    {
                        var character = (char)readValue;
                        Console.Write(character);
                    }

                    // System.Console.ReadKey() 会读取用户按下的任意键
                }
                {
                    // ## 将输出写入控制台
                    // WriteLine 是 Write 加上 \n
                    const string firstName = "lonely";
                    const string lastName = "hentai";
                    // C# 6.0 以后引入的插值字符串
                    Console.WriteLine($"My firstname is {firstName}, lastname is {lastName}");
                    // 6.0 以前的复合格式化
                    Console.WriteLine("My firstname is {0}, lastname is {1}", firstName, lastName);
                }
                // ## 注释，和 JAVA 注释格式相同，略
                {
                    // ## 应用程序接口
                    // 一种数据类型的所有方法定义了这种数据结构的应用程序接口
                    // 一个程序集所包含的所有数据类型定义了该程序集的 API
                    // 程序集的集合是框架
                }
                {
                    // ## 托管执行和公共语言基础结构
                    // 程序集采用的是公共中间语言，需要运行时进行即时编译，或者事先 AOT
                    {
                        /*
                         * CLI 公共语言基础程序中包含：
                         * - Virtual Execution System，VES 虚拟执行系统
                         * - Common Intermediate Language，CIL 公共中间语言
                         * - Common Type System，CTS 为语言互操作提供支持的类型系统，公共类型系统
                         * - Common Language Specification CLS 公共语言规范
                         * - 使各种程序能够被 CLI 识别的元数据
                         * - Base Class Library，BCL 基类库
                         */
                    }
                }
            }
        }
    }
}