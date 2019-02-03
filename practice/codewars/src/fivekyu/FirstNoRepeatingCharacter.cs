using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Codewars.Fivekyu
{
    public class FirstNoRepeatingCharacter
    {
        public static string FirstNonRepeatingLetter(string s)
        {
            var letterFormats = new Dictionary<char, char>();
            var repeatingLetters = new HashSet<char>();
            var nextLetters = new List<char>();
            var lowerString = s.ToLower();
            for (var i = 0; i < s.Length; i++)
            {
                var lowerFormat = lowerString[i];
                if (letterFormats.ContainsKey(lowerFormat))
                {
                    letterFormats.Remove(lowerFormat);
                    repeatingLetters.Add(lowerFormat);
                }
                else if (!repeatingLetters.Contains(lowerFormat))
                {
                    letterFormats[lowerFormat] = s[i];
                    nextLetters.Add(lowerFormat);
                }
            }

            var firstIndex = nextLetters.FindIndex(0, (c) => letterFormats.ContainsKey(c));
            return (firstIndex == -1 ? "" : letterFormats[nextLetters[firstIndex]].ToString());
        }

        public static string BetterFirstNonRepeatingLetter(string s)
        {
            return s.GroupBy(char.ToLower).Where(g => g.Count() == 1)
                .Select(x => x.First().ToString())
                .DefaultIfEmpty("")
                .First();
        }
    }

    [TestFixture]
    public class FirstNoRepeatingCharacterTest
    {
        [Test]
        public void BasicTests()
        {
            Assert.AreEqual("a", FirstNoRepeatingCharacter.FirstNonRepeatingLetter("a"));
            Assert.AreEqual("t", FirstNoRepeatingCharacter.FirstNonRepeatingLetter("stress"));
            Assert.AreEqual("e", FirstNoRepeatingCharacter.FirstNonRepeatingLetter("moonmen"));
        }
    }
}