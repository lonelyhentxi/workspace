using System.Collections.Generic;
using NUnit.Framework;

namespace Codewars.Sixkyu
{
    public class SplitString
    {
        public static string[] Solution(string str)
        {
            var res = new List<string> {};
            for (var i = 0; i < str.Length / 2; i++)
            {
                res.Add($"{str[2*i]}{str[2*i+1]}");
            }

            if (str.Length % 2 != 0)
            {
                res.Add($"{str[str.Length - 1]}_");
            }

            return res.ToArray();
        }
    }

    [TestFixture]
    public class SplitStringTests
    {
        [Test]
        public void BasicTests()
        {
            Assert.AreEqual(new string[] { "ab", "c_" }, SplitString.Solution("abc"));
            Assert.AreEqual(new string[] { "ab", "cd", "ef" }, SplitString.Solution("abcdef"));
        }
    }
}
