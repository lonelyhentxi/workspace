using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace essential_cs.method_param
{
    class Common
    {
        public static void Main()
        {
            {
                // # 方法的调用，总体略
                {
                    // ## 命名空间
                    /*
                     * - System 基元类型，以及用于类型转换、数学计算、程序调用以及环境管理的类型
                     * - System.Collections 包含用于处理对象集合的类型
                     * - System.Collections.Generics 包含使用泛型的强类型集合
                     * - System.Data 包含数据库处理的类型
                     * - System.Drawing 包含显示设备上进行绘图和图像处理的类型
                     * - System.IO 包含用于文件和目录处理的类型，并提供了文件的处理、加载和保存的功能
                     * - System.Linq 包含语言集成查询对集合中数据进行查询的类和接口
                     * - System.Text 用于处理字符串和各种文本编码的类型
                     * - System.Text.RegularExpression 正则
                     * - System.Threading 多线程处理
                     * - System.Threading.Tasks 基于任务的异步类型
                     * - System.Web 实现浏览器到服务器的类型
                     * - System.ServiceModel 包含在服务和客户端程序之间收发数据的类型
                     * - System.Windows 含有 WPF 创建富用户界面
                     * - System.Windows.Forms 各种组件
                     * - System.Xml 为 XML 处理提供标准的支持
                     */
                }
                // ## 类型名称、作用域、形参和实参、方法返回值略
            }
            {
                // # 方法的声明
                {
                    // ## 表达式主体方法
                    // 表达式主体方法基于 lambda 实现
                }
            }
            {
                // # using 指令
                // C# 不允许使用导入通配符
                // using 指令不允许使用嵌套命名空间
                // 可以在局部空间使用竟在作用域内的 using 指令
                // 使用 using T = T1; 的方式可以别名
            }
            {
                // # Main() 的返回值和参数
                // 见第一次章
                // 若想从非 Main 得到参数可以使用
                Console.WriteLine(string.Join(" ", GetCommandArgs()));
            }
            {
                // # 方法的参数
                // ## 值参数
                {
                    // ## ref 参数
                    // 见 swap 函数
                    string a = "a";
                    string b = "b";
                    Swap(ref a,ref b);
                    Console.WriteLine($"a: {a}, b: {b}");
                }
                {
                    // ## out 输出参数
                    // 只向变量中写入值而不读取值
                    var outTry = 0;
                    OutTry(out outTry);
                    Console.WriteLine(outTry);
                }
                {
                    // ## 参数数组
                    // 为了简化编码，C# 提供了一个特殊的关键字
                    // 允许在调用方法时提供数量可变的参数
                    PrintArgs("1","2");
                    PrintArgs(new string[]{"1","2"});
                    // 可以接受两种形式的 params，一是类似 ...的，另一是 []
                    // 必须是最后一个参数
                    // 可以指定和参数数组对应的实参
                    // 参数数组是类型安全的
                    // 调用者可以显式的使用数组
                }
            }
            // # 递归，略
            {
                // # 方法重载
                // 允许方法重载
                // 编译期多态 
                // 重载规则和 C++ 类似
            }
            {
                // # 可选参数
                // 可选参数必须要在必须参数之后
                // 还可以调用命名参数
            }
            {
                // # 用异常实现基本错误处理
                // 是常用的 try catch finally 控制流
                // 对于未处理的异常，“运行时”的行为是由具体实现定义的
                // 2.0 开始，所有异常都是从 Exception 继承
                // 没有任何参数的 catch 块称为常规 catch 块，等价于从 object 派生出的 catch 类
                {
                    // ## 使用 throw 语句报告错误
                    // throw; 可以重新抛出异常
                    // 重新 throw 会保持调用栈中的信息
                    // 而 throw exception 则会替换成当前的调用栈信息
                    // 避免使用异常来处理意料之中的情况
                    // 尽量使用 option
                }
            }
        }

        private static string[] GetCommandArgs()
        {
            return System.Environment.GetCommandLineArgs();
        }

        private static void Swap(ref string x, ref string y)
        {
            var temp = x;
            x = y;
            y = temp;
        }

        private static void OutTry(out int io)
        {
            io = 1;
        }

        private static void PrintArgs(params string[] parameters)
        {
            foreach (var param in parameters)
            {
                Console.WriteLine(param);
            }
        }
    }
}