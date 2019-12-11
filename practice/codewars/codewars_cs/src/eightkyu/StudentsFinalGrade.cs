using System;
using System.Linq;
using NUnit.Framework;

namespace Codewars
{

    public partial class Kata
    {
        public static int FinalGrade(int exam, int projects)
        {
            if (exam > 90 || projects > 10) { return 100; }
            else if (exam > 75 && projects >= 5) { return 90; }
            else if (exam > 50 && projects >= 2) { return 75; }
            else { return 0; }
        }
    }

    [TestFixture]
    public partial class Testing
    {
        [Test]
        public void FinalGradeTest()
        {
            Assert.AreEqual(Kata.FinalGrade(100, 12), 100);
            Assert.AreEqual(Kata.FinalGrade(85, 5), 90);
        }
    }
}