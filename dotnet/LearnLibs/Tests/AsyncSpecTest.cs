using System;
using System.Collections.Generic;
using System.Threading;
using System.Collections.Concurrent;
using System.Linq;
using System.Threading.Tasks;
using Xunit;
using FluentAssertions;

namespace LearnLibs.Tests
{
    public class AsyncSpecTest
    {

        [Fact]
        public void LearnBackgroundThread()
        {
            Thread t1 = new Thread(() =>
            {
                Thread.Sleep(1000);
                Assert.True(true);
            });
            Thread t2 = new Thread(new ParameterizedThreadStart((object _) =>
            {
                Thread.Sleep(2000);
                Assert.False(true);
            }));
            t2.IsBackground = true;
            t2.Start(null);
            t1.Start();
        }

        [Fact]
        public void LearnThreadPool()
        {
            var notes = new ConcurrentDictionary<int, int> { };
            WaitCallback workItem = state =>
            {
                var id = Thread.CurrentThread.ManagedThreadId;
                notes[id] = notes.GetValueOrDefault(id) + 1;
            };
            for (int i = 0; i < 10; i++)
            {
                ThreadPool.QueueUserWorkItem(workItem);
            }

            notes.Count.Should().BeLessThan(10);
        }

        [Fact]
        public void LearnParallel()
        {
            var c = new ConcurrentBag<int>();
            ParallelLoopResult result = Parallel.For(0, 1000, i => { c.Add(i); });
            c.Count.Should().Be(1000);

            var modThreeIsZero = (from num in c.AsParallel()
                where num % 2 == 0
                orderby num descending
                select num).ToList<int>();
            modThreeIsZero.Count.Should().Be(500);
        }

        [Fact]
        public void LearnTask()
        {
            var mainThreadId = Thread.CurrentThread.ManagedThreadId;
            Task.Factory.StartNew(() =>
            {
                Thread.CurrentThread.ManagedThreadId.Should().NotBe(mainThreadId);
            });
            Task.Run(() => { Thread.CurrentThread.ManagedThreadId.Should().NotBe(mainThreadId); });
        }
    }
}