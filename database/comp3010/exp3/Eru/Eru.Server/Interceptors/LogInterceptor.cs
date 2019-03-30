using Castle.DynamicProxy;
using System.Linq;
using System;
using StackExchange.Profiling;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Eru.Server.Interceptors
{
    public class LogInterceptor : IInterceptor
    {
        private static ReaderWriterLockSlim _logRwLock = new ReaderWriterLockSlim();
        private static int _logCount = 100;
        private static int _writeCount = 0;
        private static int _failedCount = 0;

        public void Intercept(IInvocation invocation)
        {
            #region CREATE_LOG_CONTENT

            var paramStrings = invocation.Arguments.Select(a => (a ?? "").ToString()).ToArray();
            var methodIndent = $"Module: {invocation.Method.Module}"
                               + $" - Method: {invocation.Method.Name} - ";
            var interceptContent = methodIndent
                                   + $"Time: {DateTime.Now:yyyy-MM-dd HH:mm:ss}."
                                   + Environment.NewLine
                                   + methodIndent
                                   + $"Params: {string.Join(", ", paramStrings)}."
                                   + Environment.NewLine;
            // let intercepted method continue
            try
            {
                MiniProfiler.Current.Step($"{methodIndent}() -> ");
                invocation.Proceed();
            }
            catch (Exception e)
            {
                MiniProfiler.Current.CustomTiming($"Error:", e.Message);
                interceptContent += $"{methodIndent}Exception:{e.Message + e.InnerException}";
            }

            interceptContent +=
                $"{methodIndent}Return: {invocation.ReturnValue}";

            #endregion

            #region WRITE_LOG_CONTENT

            Parallel.For(0, 1, e => { WriteToLog(interceptContent); });

            #endregion
        }

        static void WriteToLog(string interceptContent)
        {
        }
    }
}