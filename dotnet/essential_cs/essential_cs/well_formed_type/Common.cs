using System;
using System.IO;
using System.Collections.Generic;
using System.Text;

namespace essential_cs.well_formed_type
{
    internal class Common
    {
        public static void Main()
        {
            {
                // # 重写 object 的成员
                {
                    // ## 重写 ToString
                    // 默认情况下，在任何对象上调用 ToString 会返回类的完全限定名称
                    Console.WriteLine(new object().ToString());
                    // Console.WriteLine() 和 System.Diagnostics.Trace.Write() 等方法会调用 ToString() 
                    /*
                     * - 要返回有用的、面向开发人员的诊断字符串
                     * - 返回的字符串要简短
                     * - 不要从返回空字符串代表空
                     * - 避免引发异常或者可观察到的副作用
                     * - 格式化就要重载 ToString 或者实现 IFormattable
                     * - 考虑从 ToString 返回独一无二的字符串以标识对象实例
                     */
                    var coordinate = new Coordinate("13.1", "12.2");
                    Console.WriteLine(coordinate);
                }
                {
                    // ## 重写 GetHashCode()
                    // 重写 Equals 就要重写 GetHashCode
                    /*
                     * - 相等的对象有相等的散列码
                     * - 特定对象的生存期内，始终返回相同的值
                     * - 应该尽可能唯一
                     * - 散列码应该在范围内竟可能均匀分布
                     * - 性能应该竟可能优化
                     * - 两个对象的细微差异的应该造成散列值的极大差别
                     * - 攻击者应该难以伪造具有特定散列码的攻击
                     */
                    var coordinate = new Coordinate("13.1", "12.2");
                    Console.WriteLine(coordinate.GetHashCode());
                }
                {
                    // ## 重写 Equals
                    // 重写 Equals 而不重写 HashCode 会出现错误提示
                    var coordinate1 = new Coordinate("13.1", "12.2");
                    var coordinate2 = coordinate1;
                    Console.WriteLine($"value equals:{coordinate1.Equals(coordinate2)}");
                    Console.WriteLine($"reference equals:{ReferenceEquals(coordinate1, coordinate2)}");
                    // object 的 Equals 只是简单的调用了 ReferenceEquals
                    /*
                     * 重写 Equals 的步骤如下：
                     * - 检查是否是 null
                     * - 如果是引用类型，检查引用是否相等
                     * - 检查数据类型是否相等
                     * - 一个指定了具体类型的辅助方法，将操作数视为要比较的类型
                     * - 检查散列值是否相等
                     * - 如果基类重写了 Equals，检查 base.Equals
                     * - 比较每一个标识字段是否相等
                     * - 重写 GetHashCode()
                     * - 重写 == 和 != 操作符
                     */
                }
            }
            {
                // # 操作符重载
                // 事实上所有操作符都能够都能被重载
                // 除非目的是类型表现地像一种基元类型
                {
                    // ## 比较操作符
                    var coordinate1 = new Coordinate("13.1", "12.2");
                    var coordinate2 = coordinate1;
                    Console.WriteLine($"operator equals:{coordinate1 == coordinate2}");
                }
                // ## 二元操作符，略
                // ## 赋值和二元操作符的结合
                // ## 条件逻辑操作符
                // ## 一元操作符
                // ## 转换操作符
                {
                    // ## 转换操作符的规范
                    // 不要为有损转换提供转换操作符
                    // 不要从隐式转换中引发异常
                }
            }
            {
                // # 引用其他程序集
                // C# 和 CLI 平台不是将所有代码都放在一个二进制文件中，而是允许将代码分散到多个程序集中
                // ## 更改程序集目标，使用 /target:xxx 参数
                // ## 引用程序集，使用 /R:xxx 参数
                {
                    // ## 类型封装
                    // 默认类型修饰符是 internal
                    // internal 没法跨程序集访问
                    // protected internal 成员可以从包容程序集的任何位置以及类型的派生类中访问
                }
            }
            {
                // # 定义命名空间
                // 嵌套命名空间有普通嵌套和 point nested 嵌套
                // 包组织方式可以使用和 java 相同的方式
                // 在命令行上可以指定命名空间别名限定符 /R:alias = Module.dll
                // 对于全局作用域可以使用  global::
            }
            {
                // # XML 注释
                // XML 的声明方式前述
                {
                    // ## 生成 XML 文档文件
                    // csc /doc:Common.xml Common.cs
                }
            }
            {
                // # 垃圾回收
                // 垃圾回收只回收内存
                // .net 在 6.0 时期主要实施分代的 mark-and-compact 算法
                // 弱引用可以用来缓存
            }
            {
                // # 资源清理
                {
                    // ## 终结器
                    // 用于在被垃圾回收机制调用以清除非内存资源
                    // 终结器不允许传递参数、修饰符
                    // 避免在终结器中抛出异常
                    // 终结器不能显式调用，只有垃圾回收器才能调用终结器
                }
                {
                    // ## 使用 using 语句进行确定性终结
                    Search();
                }
                {
                    // ## 垃圾回收、终结和 IDisposable
                    // System.GC.SuppressFinalize() 是从终结队列中移除某个实例
                    // 有终结器的对象如果不被显式的释放，其生存期会被延长，即对它的所有显式引用在作用域中都消失了，终结队列中仍然包含着引用
                }
            }
            {
                // # 延迟初始化
                // 使用 lambda 以实现延迟加载
                // 为泛型和 lambda 表达式使用延迟
                // DataCache 没有注意线程安全
            }
        }

        static void Search()
        {
            using (TemporaryFileStream fileStream1 = new TemporaryFileStream(), fileStream2 = new TemporaryFileStream())
            {
                Console.WriteLine("using the temporary file stream.");
            }
        }
    }

    public class Coordinate
    {
        public Coordinate(string x, string y)
        {
            X = x;
            Y = y;
        }

        public string X { get; }
        public string Y { get; }

        public override int GetHashCode()
        {
            var hashCode = X.GetHashCode();
            var yHash = Y.GetHashCode();
            if (hashCode != yHash)
            {
                hashCode ^= yHash;
            }

            return hashCode;
        }

        public override string ToString()
        {
            return $"X:{X},Y:{Y}";
        }

        public override bool Equals(object obj)
        {
            if (obj == null)
            {
                return false;
            }

            if (this.GetType() != obj.GetType())
            {
                return false;
            }

            return Equals((Coordinate) obj);
        }

        public bool Equals(Coordinate obj)
        {
            if (null == obj)
            {
                return false;
            }

            if (ReferenceEquals(this, obj))
            {
                return true;
            }

            if (this.GetHashCode() != obj.GetHashCode())
            {
                return false;
            }

            System.Diagnostics.Debug.Assert(
                base.GetType() != typeof(object));
            if (!base.Equals(obj))
            {
                return false;
            }

            return (X.Equals(obj.X)) && (Y.Equals(obj.Y));
        }

        public static bool operator ==(
            Coordinate lhs, Coordinate rhs)
        {
            if (ReferenceEquals(lhs, null))
            {
                return ReferenceEquals(rhs, null);
            }

            return lhs.Equals(rhs);
        }

        public static bool operator !=(
            Coordinate lhs, Coordinate rhs)
        {
            return !(lhs == rhs);
        }

        public static bool operator false(Coordinate coordinate)
        {
            return coordinate == null;
        }

        public static bool operator true(Coordinate coordinate)
        {
            return coordinate != null;
        }
    }

    public class TemporaryFileStream:IDisposable
    {
        public FileStream Stream { get; }
        public FileInfo File { get; }

        public TemporaryFileStream(string fileName)
        {
            File = new FileInfo(fileName);
            Stream = new FileStream(
                File.FullName,FileMode.OpenOrCreate,
                FileAccess.ReadWrite
                );
        }

        public TemporaryFileStream()
            : this(Path.GetTempFileName()) {}

        ~TemporaryFileStream()
        {
            Dispose(false);
        }

        public void Close()
        {
            Dispose();
        }

        public void Dispose()
        {
            Dispose(true);
            System.GC.SuppressFinalize(this);
        }

        public void Dispose(bool disposing)
        {
            if (disposing)
            {
                Stream?.Close();
            }
            File?.Delete();
        }
    }

    public class DataCache
    {
        private TemporaryFileStream InternalFileStream { get; set; } = null;

        public TemporaryFileStream FileStream => InternalFileStream ?? (InternalFileStream = new TemporaryFileStream());
    }

   
    public class DataCache1
    {
        private Lazy<TemporaryFileStream> InternalFileStream { get;  }
         = new Lazy<TemporaryFileStream>(
                ()=>new TemporaryFileStream());

        public TemporaryFileStream FileStream =>
            InternalFileStream.Value;
    }
}