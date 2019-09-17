using System;
using System.Collections.Generic;
using System.Text;

namespace essential_cs.interfaces
{
    internal class Common
    {
        public static void Main()
        {
            {
                // # 接口概述
                // C# 不能为接口设定访问符
            }
            {
                // # 通过接口实现多态性
                // 可以转型成 interface，就可以调用对应的实现
            }
            {
                // # 接口实现
                // 接口不能实例化
                // 实现接口的类可以抽象实现也可以非抽象实现
                {
                    // ## 显式和隐式成员实现
                    // 声明显式接口成员名前需要加接口名前缀
                    var example = new Holy();
                    example.Foo();
                    (example as IBar).Bar();
                    /*
                     * 若某个接口和模型语义本身无关，但仍然可实现，才推荐使用显式接口成员实现
                     * - 成员不是核心的类功能，则显式实现
                     * - 接口成员名作为类成员是否恰当
                     * - 是否已经有相同签名的类成员
                     */
                }
            }
            {
                // # 在实现类和接口之间转换
                // 转换机制和继承差不多
            }
            {
                // # 接口继承
                // 接口也可以继承接口，就声明了被继承接口的所有声明
            }
            // # 多继承方法
            // # 接口上的拓展方法
            // 拓展方法也能用于接口
            // # 可以通过接口实现类似的多继承
            {
                // # 版本控制
                // 如果为已有的接口添加新的成员，可能会造成依赖的类失败
                // 避免使用标记接口，而使用特性
            }
        }
    }

    internal interface IFoo
    {
        void Foo();
    }

    internal interface IBar
    {
        void Bar();
    }

    internal class Holy : IFoo, IBar
    {
        public void Foo()
        {
            Console.WriteLine("Foo");
        }

        void IBar.Bar()
        {
            Console.WriteLine("Bar");
        }
    }
}