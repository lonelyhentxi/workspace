using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Services.Interfaces
{
    public interface IUrlService
    {
        string JoinUrlPrefix(string protocol, string domain, int? port, string basename);

        string JoinUrl(string head,string tail);

        string GetAppUrlPrefix();
    }
}
