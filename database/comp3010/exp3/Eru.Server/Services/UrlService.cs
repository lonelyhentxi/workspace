using System.Text;
using Eru.Server.Configurations;
using Eru.Server.Services.Interfaces;
using Microsoft.Extensions.Options;

namespace Eru.Server.Services
{
    public class UrlService : IUrlService
    {
        private readonly string _appPrefix;

        public UrlService(IOptions<AppConfig> appOptions)
        {
            var appConfig = appOptions.Value;
            _appPrefix = JoinUrlPrefix(appConfig.Protocol, appConfig.Domain, appConfig.Port, null);
        }

        public string JoinUrlPrefix(string protocol, string domain, int? port, string basename)
        {
            var builder = new StringBuilder();
            builder.Append(protocol ?? "https");
            builder.Append("://");
            builder.Append(domain);
            if (port != null)
            {
                builder.Append($":{port}");
            }

            builder.Append('/');
            if (!string.IsNullOrWhiteSpace(basename))
            {
                builder.Append($"{basename}");
            }

            return builder.ToString();
        }

        public string JoinUrl(string head, string tail)
        {
            if (!head.EndsWith('/'))
            {
                head += '/';
            }

            return head + tail;
        }

        public string GetAppUrlPrefix()
        {
            return _appPrefix;
        }
    }
}