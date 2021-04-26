using NUnit.Framework;

namespace Codewars
{
    public partial class Kata
    {
        public static int[] GenerateRange(int min, int max, int step)
        {
            var ranges = new int [(max-min)/step+1];
            for (int i = min, j=0; i <= max; i += step,j++)
            {
                ranges[j] = i;
            }
            return ranges;
        }
    }



    [TestFixture]
    public partial class Testing
    {
        [Test]
        [TestCase(2, 10, 2, ExpectedResult = new int[] { 2, 4, 6, 8, 10 })]
        public static int[] FixedTest(int min, int max, int step)
        {
            return Kata.GenerateRange(min, max, step);
        }
    }
}