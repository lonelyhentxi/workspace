using System;
using System.IO;

namespace LearnLibs
{
    static class RepoUtils
    {
        public static void FsSafeCopy(string relativePath, string sourceBase, string targetBase)
        {
            var sourceFullPath = Path.Combine(sourceBase, relativePath);
            var targetFullPath = Path.Combine(targetBase, relativePath);
            var targetDirectory = Path.GetDirectoryName(targetFullPath);
            if (!Directory.Exists(targetDirectory))
            {
                Directory.CreateDirectory(targetDirectory);
            }

            if (File.Exists(sourceFullPath))
            {
                File.Create(targetFullPath).Close();
                File.Copy(sourceFullPath, targetFullPath, true);
            }
            else if (Directory.Exists(sourceFullPath))
            {
                Directory.CreateDirectory(targetFullPath);
            }
        }

        private static void _fsSafeDeleteFile(string path)
        {
            File.SetAttributes(path, FileAttributes.Normal);
            File.Delete(path);
        }

        private static void _fsSafeDelete(string path)
        {
            if (File.Exists(path))
            {
                _fsSafeDeleteFile(path);
            }
            else if (Directory.Exists(path))
            {
                foreach (var e in Directory.EnumerateFileSystemEntries(path))
                {
                    _fsSafeDelete(Path.Combine(path, e));
                }

                Directory.Delete(path, true);
            }
        }

        public static void FsSafeDelete(string path)
        {
            try
            {
                if (File.Exists(path))
                {
                    File.Delete(path);
                }
                else if (Directory.Exists(path))
                {
                    Directory.Delete(path, true);
                }
            }
            catch (UnauthorizedAccessException _)
            {
                _fsSafeDelete(path);
            }
        }
    }
}
