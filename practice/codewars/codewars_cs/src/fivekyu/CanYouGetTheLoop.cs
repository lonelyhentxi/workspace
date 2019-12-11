using NUnit.Framework;
using System.Collections.Generic;

namespace Codewars.Fivekyu
{
    public class LoopDetector
    {
        public class Node
        {
            public Node next { set; get; }
        }
    }

    public class CanYouGetTheLoop
    {
        public static int getLoopSize(LoopDetector.Node startNode)
        {
            var notebook = new Dictionary<LoopDetector.Node, int>();
            var current = startNode;
            var nodeIndex = 0;
            while (true)
            {
                if (current is null)
                {
                    return 0;
                }

                if (notebook.ContainsKey(current))
                {
                    return nodeIndex - notebook[current];
                }
                else
                {
                    notebook[current] = nodeIndex;
                }

                nodeIndex += 1;
                current = current.next;
            }
        }
    }

    [TestFixture]
    public class CanYouGetTheLoopTest
    {
        [Test]
        public void FourNodesWithLoopSize3()
        {
            var n1 = new LoopDetector.Node();
            var n2 = new LoopDetector.Node();
            var n3 = new LoopDetector.Node();
            var n4 = new LoopDetector.Node();
            n1.next = n2;
            n2.next = n3;
            n3.next = n4;
            n4.next = n2;
            Assert.AreEqual(3, CanYouGetTheLoop.getLoopSize(n1));
        }
    }
}
