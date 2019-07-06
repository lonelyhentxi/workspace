using System;
using System.Linq;
using NUnit.Framework;

namespace Codewars.Fourkyu
{
    public class NextBiggerNumberWithTheSameDigits
    {
        public static long NextBiggerNumber(long n)
        {
            var digits = n.ToString().ToArray().Select(ch => int.Parse(ch.ToString())).ToArray();
            int i=0;
            for (i = digits.Length - 1; i > 0; i--)
            {
                if (digits[i] > digits[i - 1])
                {
                    break;
                }
            }

            if (i == 0)
            {
                return -1;
            }
            else
            {
                var x = digits[i - 1];
                var min = i;
                for (var j = i + 1; j < digits.Length; j++)
                {
                    if (digits[j] > x && digits[j] < digits[min])
                    {
                        min = j;
                    }
                }

                var temp = digits[i - 1];
                digits[i - 1] = digits[min];
                digits[min] = temp;
                Array.Sort(digits,i,digits.Length-i);
                return long.Parse(string.Join("", digits.Select(d => d.ToString())));
            }
        }
    }

    [TestFixture]
    public class NextBiggerNumberTests
    {
        [Test]
        public void Test1()
        {
            Assert.AreEqual(21, NextBiggerNumberWithTheSameDigits.NextBiggerNumber(12));
            Assert.AreEqual(531, NextBiggerNumberWithTheSameDigits.NextBiggerNumber(513));
            Assert.AreEqual(2071, NextBiggerNumberWithTheSameDigits.NextBiggerNumber(2017));
            Assert.AreEqual(441, NextBiggerNumberWithTheSameDigits.NextBiggerNumber(414));
            Assert.AreEqual(414, NextBiggerNumberWithTheSameDigits.NextBiggerNumber(144));
        }
    }
}
