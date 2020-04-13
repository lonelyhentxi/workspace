using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace essential_cs.derive
{
    internal class Common
    {
        public static void Main()
        {
            // 继承建立了 “属于” 关系；派生类型总是隐式地属于基类型。
            {
                // # 派生
                {
                    // 未指定的类型继承于 object
                    // 子类型到基类型可以发生隐式转换
                    // 基类型到子类型必须明确的请求显式转换
                    var b = new B();
                    // 定义 B 到 A 的隐式转型
                    A a = b;
                }
                {
                    // ## private 和 protected 访问修饰符
                    // 派生类继承了除构造器和析构器之外的所有成员
                    // 派生类不能访问基类的 private 成员
                    // 只有派生类才能访问基类的 protected 成员
                }
                {
                    // ## 拓展方法从技术上不是类型的成员
                    // 但是派生类方法可以调用基类方法，可以使用
                    // 继承链上的同名方法会覆盖拓展方法
                }
                // C# 和 Java 同样是单继承
                // 可以用聚合解决多继承问题
                // ## 密封类，使用 sealed 标记，略
            }
            {
                // # 基类的重写
                {
                    // ## virtual 修饰符
                    // C# 支持重写实例方法和属性，但不支持重写字段或者任何静态成员。
                    // 需要将允许重写的类型标记为 virtual
                    // ps：与之相比，Java 默认虚方法
                    // C# 要求覆盖的方法必须是 override
                    // 不要在构造器中调用会影响调用对象的任何虚方法。
                    // C++ 中，构造期间不调度虚算法，在构造期间，类型与基类型相关联。
                    // C# 会将虚方法调度给派生得最远的类型
                }
                {
                    // ## new 修饰符
                    // 假如既没有指定 override ，也没有 new，则会默认 new，保持版本安全性
                    // 使用 new 操作符的方法，在基类面前隐藏了派生类重新声明的成员
                    // 搜索继承连，找到使用 new 修饰符的那个成员之前的成员，然后调用该成员
                    SuperSubDerive s = new SuperSubDerive();
                    SubDerive s1 = s;
                    Derive s2 = s;
                    Base s3 = s;
                    s.DisplayName();
                    s1.DisplayName();
                    s2.DisplayName();
                    s3.DisplayName();
                }
                {
                }
                {
                    // ## sealed 修饰符
                    // 使用 sealed 修饰符修饰一个方法，可以禁止重载这个成员
                }
                {
                    // ## base 成员
                    // 使用 base.xxx 可以调用基类成员
                }
                {
                    // ## 构造器
                    // 使用 base(xxx) 可以调用基类构造器
                }
            }
            {
                // # 抽象类
                // 无法实例的类一定是抽象类
                // C# 强制要求为类定义添加 abstract 修饰符
                // 重要的是含有 abstract 方法，其只有声明没有实现
                // 抽象成员不能是私有
            }
            {
                // # 使用 is 操作符验证基础类型
                var s = new SubDerive();
                Console.WriteLine(s is Derive);
            }
            {
                // # 使用 as 操作符进行转换
                // as 会检查并转换一个类型，如果不能转换会返回 null
                var s = new SubDerive();
                var d = s as Derive;
                d.DisplayName();
            }
        }
    }

    internal class A
    {
        public A()
        {
        }
    }

    internal class B
    {
        public B()
        {
        }

        // 写成 explicit 就是显式转型
        public static implicit operator A(B b)
        {
            return new A();
        }
    }

    public class Base
    {
        public void DisplayName()
        {
            Console.WriteLine("BaseClass");
        }
    }

    public class Derive : Base
    {
        public new virtual void DisplayName()
        {
            Console.WriteLine("DeriveClass");
        }
    }

    public class SubDerive : Derive
    {
        public override void DisplayName()
        {
            Console.WriteLine("SubDeriveClass");
        }
    }

    public class SuperSubDerive : SubDerive
    {
        public new void DisplayName()
        {
            Console.WriteLine("SuperSubDeriveClass");
        }
    }
}