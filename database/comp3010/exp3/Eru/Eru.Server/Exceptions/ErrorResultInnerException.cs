using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Exceptions
{
    public class ErrorResultInnerException
    {
        public Exception inner { get; set; }
        public string message { get; set; }

        public dynamic body { get; set; }
    }
}
