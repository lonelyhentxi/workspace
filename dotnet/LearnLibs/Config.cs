using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace LearnLibs
{
    static class Config
    {
        public static readonly string ProjectRoot =
            Path.GetDirectoryName(Path.GetDirectoryName(Path.GetDirectoryName(Directory.GetCurrentDirectory())));
        public static readonly string ResourceRoot = Path.Combine(ProjectRoot, "Resources");
        public static readonly string RepoRoot = Path.Combine(ResourceRoot, "Repos");
    }
}
