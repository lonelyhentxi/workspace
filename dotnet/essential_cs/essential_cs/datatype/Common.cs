using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace essential_cs.datatype
{
    class Common
    {
        public static void Main()
        {
            {
                // # 基本数值类型
                {
                    // ## 整数类型
                    // sbyte，byte，short，ushort，int，uint，long，ulong 是 BCL 类型缩写
                }
                {
                    // ## 浮点类型
                    // float 32 位
                    // double 64 位
                    // decimal 十进制浮点数 128 位
                    // 除非超过范围，否则 decimal 表示的值是一定准确的，适合用来精准计算
                }
                {
                    // ## 字面量
                    Console.WriteLine(6.023E23F);
                    Console.WriteLine(0x002A);
                    Console.WriteLine($"0x{42:X}"); // :X 表示字符串插值表达式
                    {
                        // round-trip 格式化
                        const double number = 1.618033988749895;
                        double result;
                        string text;
                        text = $"{number}";
                        result = double.Parse(text);
                        Console.WriteLine($"{result == number}: result == number");
                        // 将 round-trip 格式说明符返回的字符转换会数值肯定能获得原始值
                        text = string.Format("{0:R}", number);
                        result = double.Parse(text);
                        Console.WriteLine($"{result == number}: result == number");
                    }
                }
            }
            {
                // # 更多基本类型
                // ## 布尔类型，略
                {
                    // ## 字符类型
                    // char 表示 16 位 Unicode 字符
                }
                {
                    // ## 字符串
                    // 逐字字符串
                    Console.WriteLine(@"
                              |-------|
                    ");
                    // 插值字符串见前方
                    // 逐字字符串和插值字符串可以组合
                    // 字符串插值是调用 string.Format() 方法的简写
                    // 字符串方法
                    // Format
                    Console.WriteLine(string.Format("Holy {0}!", "shit"));
                    // Concat
                    Console.WriteLine(string.Concat("str1 ", " str2"));
                    // Compare
                    Console.WriteLine(string.Compare("abc", "Abc", true, CultureInfo.CurrentCulture) == 0);
                    // StartsWith, EndsWidth
                    Console.WriteLine("abcd".StartsWith("abc"));
                    // ToUpper, ToLower
                    Console.WriteLine("abc".ToUpper());
                    // Trim, TrimEnd, TrimStart, TrimEnd
                    Console.WriteLine("--User--".TrimStart('-'));
                    // Replace
                    Console.WriteLine("abc".Replace("a", "A"));
                }
                // 一般静态方法的调用要携带类型
                // 使用 using static 指令
                {
                    // 字符串格式
                    Console.WriteLine($"{0.20:C2}", 0);
                    // 具体查阅 MSDN
                }
                {
                    // 换行符
                    Console.Write("{0}{1}", "This is a line.", Environment.NewLine);
                }
                {
                    // 字符串长度
                    Console.WriteLine("abc".Length);
                }
                // 字符串是不可变的
                // 如果有大量的字符串要修改可以使用 StringBuilder
            }
            {
                // # null 和 void
                // ## null，略
                // ## void，指出没有返回类型
                // ## var 类似于其他语言的 var、auto 等
            }
            {
                // # 类型的分类
                // 值类型和引用类型，略
            }
            {
                // # 可空修饰符
                // 使用可空操作符可以使得值类型能够被赋给 null
            }
            {
                // # 数据类型之间的转换
                // 可能变大变小或者引发异常需要显式转型
                // 其他属于隐式转型
                {
                    // ## 显式转型
                    // 普通的算术溢出不会引发问题
                    // 使用 checked 块会进行检查
                    checked
                    {
                        try
                        {
                            int n = int.MaxValue;
                            n = n + 1;
                            Console.WriteLine(n);
                        }
                        catch (Exception e)
                        {
                            Console.WriteLine(e);
                        }
                    }

                    unchecked
                    {
                        int n = int.MaxValue;
                        n = n + 1;
                        Console.WriteLine(n);
                    }
                }
                // 不存在从数值型到 boolean 类型的转型
                {
                    // ## 隐式转型
                    // 隐式转型无需使用转型操作符
                }
                {
                    // ## 不使用转型操作符的类型转换
                    string text = "9.11E-31";
                    float k = float.Parse(text);
                    Console.WriteLine(k);
                    string middleCText = "261.626";
                    bool boolean = Convert.ToBoolean(middleCText);
                    Console.WriteLine(boolean);
                    // 或者 ToString
                    // 从 C# 2.0 开始，TryCase() 被所有基元类型包含，不会抛出异常
                }
            }
            {
                // # 数组类型
                // 除泛型外基本和 Java 中相同
                // 但是方括号是跟在类型之后的
                // new 中可以类型推断
                // 可以使用列表（嵌套列表）初始化
                // default 可以得到类型默认值
                // 大小不一致的多维数组会造成错误
                {
                    // ## 更多数组方法
                    // 例如 Sort、BinarySearch、Reverse、Clear 等
                    // GetLength、Rank、Clone 等
                    // 字符串可以当做不可变的近似一维数组使用
                }
            }
        }
    }
}