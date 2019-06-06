using System;
using System.IO;
using System.Management.Automation;
using System.Threading.Tasks;
using LibGit2Sharp;
using Xunit;
using System.Linq;

namespace LearnLibs.Tests
{
    public class Libgit2Learn
    {
        [Fact]
        public async Task ShouldClone()
        {
            if (Directory.Exists(Path.Combine(Config.RepoRoot, "clone-test")))
            {
                RepoUtils.FsSafeDelete(Path.Combine(Config.RepoRoot, "clone-test"));
            }
            using var powershell = PowerShell.Create();
            powershell.AddScript($"cd {Config.RepoRoot}");
            powershell.AddScript(
                $"$env:GIT_SSH_COMMAND=\"ssh -i {Path.Combine(Config.ResourceRoot, "Ssh", "id-ed25519")}\"");
            powershell.AddScript(
                $@"git clone ssh://git@gitea.evernightfireworks.com:7002/lonelyhentai/probabel-test.git -b master clone-test -q");
            powershell.AddScript("exit");
            await powershell.InvokeAsync();
            Assert.True(Directory.Exists(Path.Combine(Config.RepoRoot, "clone-test")));
        }

        [Fact]
        public async Task ShouldDiff()
        {
            await ShouldClone();
            using var repo = new Repository(Path.Combine(Config.RepoRoot,"clone-test"));
            var currentCommit = repo.Lookup<Commit>("b11f6bc38ff60b10d61ee9d99d8c0b6959fdcb96");
            var lastCommit = repo.Lookup<Commit>("77e8c01d821d626ddc09c913bbc28de068c31ffd");
            var differences = repo.Diff.Compare<Patch>(lastCommit.Tree,currentCommit.Tree);
            Assert.True(differences.FirstOrDefault(e => e.Path.Contains("i18n-src.js"))!=null);
        }

        [Fact]
        public void ShouldInit()
        {
            var repoPath = Path.Combine(Config.RepoRoot, "init-test");
            if (Directory.Exists(repoPath)) RepoUtils.FsSafeDelete(repoPath);
            Repository.Init(repoPath, false);
            Assert.True(Directory.Exists(Path.Combine(repoPath, ".git")));
        }

        [Fact]
        public async Task ShouldCommit()
        {
            await ShouldClone();
            ShouldInit();
            using var sourceRepo = new Repository(Path.Combine(Config.RepoRoot, "clone-test"));
            using var targetRepo = new Repository(Path.Combine(Config.RepoRoot, "init-test"));
            var lastCommit = sourceRepo.Lookup<Commit>("77e8c01d821d626ddc09c913bbc28de068c31ffd");
            sourceRepo.Reset(ResetMode.Hard, lastCommit);
            RepoUtils.FsSafeDelete(Path.Combine(Config.RepoRoot, "init-test", "zh-cn"));
            RepoUtils.FsSafeCopy(Path.Combine("mix_i18n", "i18n-src.js"), Path.Combine(Config.RepoRoot, "clone-test"),
                Path.Combine(Config.RepoRoot, "init-test","zh-cn"));
            RepoUtils.FsSafeCopy(Path.Combine("pure_i18n", "i18n-zh-cn.json"), Path.Combine(Config.RepoRoot, "clone-test"),
                Path.Combine(Config.RepoRoot, "init-test","zh-cn"));
            Commands.Stage(targetRepo, "*");
            var author = new LibGit2Sharp.Signature("lonelyhentai", "master@evernightfireworks.com", DateTime.Now);
            var committer = author;
            var commit = targetRepo.Commit("Here's a commit i made!", author, committer);
            var currentCommit = sourceRepo.Lookup<Commit>("b11f6bc38ff60b10d61ee9d99d8c0b6959fdcb96");
            sourceRepo.Reset(ResetMode.Hard, currentCommit);
        }
    }
}