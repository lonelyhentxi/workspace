using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Codewars.Fivekyu
{
    public class DirectionsReduction
    {
        public static string[] DirReduce(string[] arr)
        {
            var directionDict = new Dictionary<string, string>
                {{"NORTH", "SOUTH"}, {"SOUTH", "NORTH"}, {"EAST", "WEST"}, {"WEST", "EAST"}};
            var directionLinks = new LinkedList<string>(arr);
            var current = directionLinks.First;
            LinkedListNode<string> prev = null;
            while (current!=null)
            {
                if (directionDict[current.Value] == prev?.Value)
                {
                    var next = current.Next;
                    directionLinks.Remove(current);
                    current = next;
                    // here prev can't be null
                    var prevPrev = prev.Previous;
                    directionLinks.Remove(prev);
                    prev = prevPrev;
                }
                else
                {
                    prev = current;
                    current = current.Next;
                }
            }

            return directionLinks.ToArray();
        }
    }

    [TestFixture]
    public class DirectionsReductionTests
    {

        [Test]
        public void Test1()
        {
            string[] a = new string[] { "NORTH", "SOUTH", "SOUTH", "EAST", "WEST", "NORTH", "WEST" };
            string[] b = new string[] { "WEST" };
            Assert.AreEqual(b, DirectionsReduction.DirReduce(a));
        }
        [Test]
        public void Test2()
        {
            string[] a = new string[] { "NORTH", "WEST", "SOUTH", "EAST" };
            string[] b = new string[] { "NORTH", "WEST", "SOUTH", "EAST" };
            Assert.AreEqual(b, DirectionsReduction.DirReduce(a));
        }
    }
}
