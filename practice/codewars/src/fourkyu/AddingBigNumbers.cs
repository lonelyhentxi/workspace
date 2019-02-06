using System;
using System.Linq;
using NUnit.Framework;

namespace Codewars.Fourkyu
{

    public class PositiveBigInteger
    {
        private readonly uint[] _bits;
        private static readonly int UnitLength = uint.MaxValue.ToString().Length-1;
        private static readonly uint MaxUnits = (uint)Math.Pow(10, UnitLength);
        private static readonly string ToStringFormat = $"{{0:D{UnitLength}}}";

        public PositiveBigInteger(uint[] value)
        {
            _bits = value;
        }

        public static PositiveBigInteger Parse(string s)
        {
            return Parse((ReadOnlySpan<char>)s);
        }

        public static PositiveBigInteger Parse(ReadOnlySpan<char> value)
        {
            if (!ParseArgumentValidate(value, out ArgumentException e))
            {
                throw e;
            }

            var units = (int) Math.Ceiling(value.Length / (float) UnitLength);
            var bits = new uint[units];
            var highestBitsUnitLength = value.Length - (units - 1) * UnitLength;
            bits[units - 1] = uint.Parse(value.Slice(0, highestBitsUnitLength));
            for (int i=highestBitsUnitLength, bitsIndex = units-2; i < value.Length; i+=UnitLength,bitsIndex--)
            {
                bits[bitsIndex] = uint.Parse(value.Slice(i, UnitLength));
            }

            var lastUnitStart = (units - 1) * UnitLength;

            return new PositiveBigInteger(bits);
        }

        private static bool ParseArgumentValidate(ReadOnlySpan<char> value,out ArgumentException error)
        {
            if (value == null)
            {
                error = new ArgumentNullException();
                return false;
            }
            if (value.Length == 0)
            {
                error = new ArgumentException($"{nameof(PositiveBigInteger)} should not empty.");
                return false;
            }
            foreach(var ch in value)
            {
                if (!char.IsDigit(ch))
                {
                    error = new ArgumentException($"{nameof(PositiveBigInteger)} should not include not-digits chars.");
                    return false;
                }
            }
            error = null;
            return true;
        }

        public static PositiveBigInteger Add(PositiveBigInteger lhs, PositiveBigInteger rhs)
        {
            var minLength = Math.Min(lhs._bits.Length, rhs._bits.Length);
            var maxLength = Math.Max(lhs._bits.Length, rhs._bits.Length);
            var sumBits = new uint[maxLength];
            var longerBits = maxLength == lhs._bits.Length ? lhs._bits : rhs._bits;
            uint carry = 0;
            for (var i = 0; i < minLength; i++)
            {
                var currentSum = lhs._bits[i] + rhs._bits[i] + carry;
                if (currentSum >= MaxUnits)
                {
                    sumBits[i] = currentSum - MaxUnits;
                    carry = 1;
                }
                else
                {
                    sumBits[i] = currentSum;
                    carry = 0;
                }
            }

            for (var i = minLength; i < maxLength; i++)
            {
                var currentSum = longerBits[i] + carry;
                if (currentSum >= MaxUnits)
                {
                    sumBits[i] = currentSum - MaxUnits;
                    carry = 1;
                }
                else
                {
                    sumBits[i] = currentSum;
                    carry = 0;
                }
            }

            if (carry == 1)
            {
                var oldSumBits = sumBits;
                sumBits = new uint[oldSumBits.Length + 1];
                for (var i = 0; i < oldSumBits.Length; i++)
                {
                    sumBits[i] = oldSumBits[i];
                }

                sumBits[sumBits.Length - 1] = 1;
            }

            return new PositiveBigInteger(sumBits);
        }

        public override string ToString()
        {
            return string.Join("", _bits.Select(unit => string.Format(ToStringFormat,unit)).Reverse()).TrimStart('0');
        }
    }

    public static class AddingBigNumber
    {
        public static string Add(string a, string b)
        {
            var lhs = PositiveBigInteger.Parse(a);
            var rhs = PositiveBigInteger.Parse(b);
            var sum = PositiveBigInteger.Add(lhs, rhs);
            return sum.ToString();
        }
    }

    [TestFixture]
    public class PositiveBigIntegerTest
    {
        [Test]
        public static void Test1()
        {
            Assert.AreEqual("444",AddingBigNumber.Add("123","321"));
            Assert.AreEqual("110", AddingBigNumber.Add("99", "11"));
        }

        [Test]
        public static void BigNumberTest()
        {
            Assert.AreEqual("1111111111", AddingBigNumber.Add("123456789", "987654322"));
            Assert.AreEqual("1057853509440367665682450458794866464501746580388666517943654",
                AddingBigNumber.Add("823094582094385190384102934810293481029348123094818923749817",
                    "234758927345982475298347523984572983472398457293847594193837"));
        }
    }
}