using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Exceptions
{
    public class NotExistedException: ApplicationException
    {
        public NotExistedException() : base() { }
        public NotExistedException(string message) : base(message) { }
        public NotExistedException(string message, Exception inner) : base(message, inner) { }
    }
}
