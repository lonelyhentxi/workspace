using System;
using System.IO;
using System.Management.Automation;
using System.Threading;
using System.Threading.Tasks;
using LibGit2Sharp;
using Xunit;

namespace LearnLibs.Tests
{
    public class Libgit2Learn
    {
        public Libgit2Learn()
        {
        }

        [Fact]
        public void ShouldInit()
        {
            var repoPath = Path.Combine(Config.RepoRoot, "init-test");
            if(Directory.Exists(repoPath)) Directory.Delete(repoPath,recursive:true);
            Repository.Init(repoPath, false);
            Assert.True(Directory.Exists(Path.Combine(repoPath, ".git")));
        }

        [Fact]
        public async Task ShouldClone()
        {
            using var powershell = PowerShell.Create();
            powershell.AddScript($"cd {Config.RepoRoot}");
            powershell.AddScript(
                $"$env:GIT_SSH_COMMAND=\"ssh -i {Path.Combine(Config.ResourceRoot, "Ssh", "id-ed25519")}\"");
            powershell.AddScript(
                $@"git clone git@github.com:lonelyhentai/enfw-plugins.git -b master clone-test -q");
            powershell.AddScript("exit");
            await powershell.InvokeAsync();
            Assert.True(Directory.Exists(Path.Combine(Config.RepoRoot, "clone-test")));
        }
    }
}