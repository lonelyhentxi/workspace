using System;
using NUnit.Framework;

namespace Codewars.SevenKyu
{
    public static class SuitcasePacking
    {
        public static bool FitSquares(int a, int b, int m, int n)
        {
            var suitcaseLength = Math.Max(m, n);
            var suitcaseWidth = Math.Min(m, n);
            return (a + b <= suitcaseLength) && (Math.Max(a, b) <= suitcaseWidth);
        }
    }

    [TestFixture]
    public static class TestSuitcasePacking
    {
        [Test]
        public static void TestFitSquare()
        {
            Assert.AreEqual(true, SuitcasePacking.FitSquares(1, 2, 3, 2));
            Assert.AreEqual(false, SuitcasePacking.FitSquares(1, 2, 2, 1));
            Assert.AreEqual(false, SuitcasePacking.FitSquares(3, 2, 3, 2));
            Assert.AreEqual(false, SuitcasePacking.FitSquares(1, 2, 1, 2));
            Assert.AreEqual(false, SuitcasePacking.FitSquares(6, 5, 8, 7));
        }
    }
}
