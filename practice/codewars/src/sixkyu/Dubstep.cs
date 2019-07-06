using System;
using System.Linq;
using NUnit.Framework;

namespace Codewars.Sixkyu
{
    public class Dubstep
    {
        public static string SongDecoder(string input)
        {
            var nonEmptySplits = input.Split("WUB", StringSplitOptions.None).Where((x)=>x!="").ToList();
            return string.Join(" ", nonEmptySplits);
        }
    }

    [TestFixture]
    public class DupStepTest
    {
        [Test]
        [TestCase("WUBWUBABCWUB", ExpectedResult = "ABC")]
        [TestCase("RWUBWUBWUBLWUB", ExpectedResult = "R L")]
        public static string BaseTest(string input)
        {
            return Dubstep.SongDecoder(input);
        }
    }
}
