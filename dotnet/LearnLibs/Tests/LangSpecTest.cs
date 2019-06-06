using Xunit;

namespace LearnLibs.Tests
{
    public class LangSpecTest
    {
        public delegate int SumDelegate();
        /// <summary>
        /// from 7.0
        /// </summary>
        [Fact]
        public void LearnScopeFunction()
        {
            int Sum() => 1 + 2;
            var sumDelegate = (SumDelegate) Sum;
            Assert.Equal(3, sumDelegate());
        }

        [Fact]
        public void LearnAnonymousDelegate()
        {
            SumDelegate delegater = delegate() { return 1 + 2; };
            SumDelegate delegater1 = () => 1 + 2;
            Assert.Equal(delegater1(), delegater());
        }
    }
}
