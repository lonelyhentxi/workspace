using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Codewars.Sixkyu
{
    public class EnoughIsEnough
    {
        public static int[] DeleteNth(int[] arr, int x)
        {
            if (x < 1)
            {
                throw new ArgumentException("x should not less than 1");
            }

            var preparations = new Dictionary<int, int>();
            var res = new List<int>();
            foreach (var i in arr)
            {
                if (!preparations.ContainsKey(i))
                {
                    res.Add(i);
                    preparations[i] = 1;
                }
                else if (preparations[i] < x)
                {
                    res.Add(i);
                    preparations[i] = preparations[i] + 1;
                }
            }
            return res.ToArray();
        }
    }

    [TestFixture]
    public class EnoughIsEnoughTest
    {
        [Test]
        public void TestSimple()
        {
            var expected = new int[] {20, 37, 21};

            var actual = EnoughIsEnough.DeleteNth(new int[] {20, 37, 20, 21}, 1);

            CollectionAssert.AreEqual(expected, actual);
        }

        [Test]
        public void TestSimple2()
        {
            var expected = new int[] {1, 1, 3, 3, 7, 2, 2, 2};

            var actual = EnoughIsEnough.DeleteNth(new int[] {1, 1, 3, 3, 7, 2, 2, 2, 2}, 3);

            CollectionAssert.AreEqual(expected, actual);
        }
    }
}