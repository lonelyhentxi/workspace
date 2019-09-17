using System;
using System.Collections.Generic;
using System.Text;

namespace essential_cs.structs
{
    internal class Common
    {
        public static void Main()
        {
            // 值类型在栈上分配，引用类型在堆上分配（不绝对，可能后期优化有逃逸分析）
            // 见 C++
            {
                // # 结构
                // struct 声明
                // 虽然语言未做要求，但是值类型最好是不可修改的，和其语义相符合
                {
                    // ## 结构的初始化
                    // 结构允许方法和构造器
                    // 结构不允许自定义无参数构造器
                    // 编译器提供产生一个默认构造器将所有字段初始化其默认值
                    // C# 禁止结构字段初始化器
                    // 不初始化所有字段就访问 this 时非法的
                    // 当实例化一个未赋值的值类型字段的引用类型，或者实例化一个没有初始化器的值类型数组时，应该显式地初始化
                    // 结构不具有终结器
                }
                {
                    // ## default 操作符的使用
                    var a = default(int);
                    Console.WriteLine(a);
                }
                {
                    // ## 值类型的继承和接口
                    // 所有的值类型都继承自 object - System.Valuetype - 结构
                    // 值类型可以实现接口
                }
            }
            {
                // # 装箱
                // 当值类型转换到一个引用类型时，会发生装箱
                // 可变的值类型装箱后修改再拆箱后可能修改失败
                // 非可变类型可以阻止过多的装箱
                /*
                 * 若是修改值类型的值，必须调用时是引用类型
                 */
            }
            {
                // # 枚举
                // 所有枚举类型的基类是 Enum
                Console.WriteLine($"a:{(short)ABC.A},b:{(short)ABC.B},c:{(short)ABC.C}");
                // C# 不支持枚举类型转型，CLR 支持
                // 枚举类型的 ToString 会产生其名字字符串
                // 可以使用 Enum.Parse 方法将字符串转换为 Enum
                // 这种转换不支持本地化
                // 将枚举值作为标志使用，定义时使用位移操作符并按位或或者与，检测时使用按位或或者与操作符
                // 决定使用 flag 的 enum 需要使用 [Flags] 标记，改变了 TryParse 和 ToString 的行为
            }
        }
    }

    internal struct Angle
    {
        public Angle(int degrees)
        {
            Degrees = degrees;
        }

        readonly int Degrees;

        public Angle Move(int degrees)
        {
            return new Angle(Degrees + degrees);
        }
    }

    internal enum ABC : short
    {
        A,
        B = 10,
        C
    }
}
