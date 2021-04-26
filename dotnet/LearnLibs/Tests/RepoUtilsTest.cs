using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Xunit;

namespace LearnLibs.Tests
{
    public class RepoUtilsTest
    {
        [Fact]
        public void ShouldSafeCopy()
        {
            Directory.CreateDirectory(Path.Combine(Config.RepoRoot, "source-test","mid","inner"));
            Directory.CreateDirectory(Path.Combine(Config.RepoRoot, "source-test", "mid-1"));
            var f = File.Create(Path.Combine(Config.RepoRoot, "source-test", "mid-1", "abc.txt"));
            f.Close();
            RepoUtils.FsSafeCopy(Path.Combine("mid", "inner"), Path.Combine(Config.RepoRoot, "source-test"),
                Path.Combine(Config.RepoRoot, "target-test"));
            RepoUtils.FsSafeCopy(Path.Combine("mid-1", "abc.txt"),
                Path.Combine(Config.RepoRoot, "source-test"),
                Path.Combine(Config.RepoRoot, "target-test"));
            Assert.True(File.Exists(Path.Combine(Config.RepoRoot, "target-test", "mid-1", "abc.txt")));
            Assert.True(Directory.Exists(Path.Combine(Config.RepoRoot, "source-test", "mid", "inner")));
            Directory.Delete(Path.Combine(Config.RepoRoot, "source-test"), true);
            Directory.Delete(Path.Combine(Config.RepoRoot, "target-test"), true);
        }

        [Fact]
        public void ShouldSafeDelete()
        {
            Directory.CreateDirectory(Path.Combine(Config.RepoRoot, "source-test", "mid", "inner"));
            Directory.CreateDirectory(Path.Combine(Config.RepoRoot, "source-test", "mid-1"));
            var f = File.Create(Path.Combine(Config.RepoRoot, "source-test", "mid-1", "abc.txt"));
            f.Close();
            RepoUtils.FsSafeDelete(Path.Combine(Config.RepoRoot, "source-test", "mid-1", "abc.txt"));
            RepoUtils.FsSafeDelete(Path.Combine(Config.RepoRoot, "source-test"));
            Assert.False(Directory.Exists(Path.Combine(Config.RepoRoot, "source-test")));
        }
    }
}
