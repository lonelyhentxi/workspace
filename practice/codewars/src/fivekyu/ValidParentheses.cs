using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace Codewars.Fivekyu
{
    public static class Parentheses
    {
        public static readonly HashSet<char> OpenParentheses = new HashSet<char> {'(', '[', '<'};
        public static readonly HashSet<char> CloseParentheses = new HashSet<char> {')', ']', '>'};

        public static readonly Dictionary<char, char> Pairs = new Dictionary<char, char>
            {{'(', ')'}, {')', '('}, {'[', ']'}, {']', '['}, {'>', '<'}, {'<', '>'}};

        public static bool ValidParentheses(string input)
        {
            var stack = new Stack<char>();
            foreach (var ch in input)
            {
                if (OpenParentheses.Contains(ch))
                {
                    stack.Push(ch);
                }
                else if (CloseParentheses.Contains(ch))
                {
                    try
                    {
                        var last = stack.Pop();
                        if (Pairs[ch] != last)
                        {
                            return false;
                        }
                    }
                    catch(InvalidOperationException)
                    {
                        return false;
                    }
                }
            }

            return stack.Count == 0;
        }
    }

    [TestFixture]
    public class ParenthesesTest
    {
        [Test]
        public void SampleTest1()
        {
            Assert.AreEqual(true, Parentheses.ValidParentheses("()"));
        }

        [Test]
        public void SampleTest2()
        {
            Assert.AreEqual(false, Parentheses.ValidParentheses(")(((("));
        }
    }
}