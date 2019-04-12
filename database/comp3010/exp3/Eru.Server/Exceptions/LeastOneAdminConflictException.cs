using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Exceptions
{
    public class LeastOneAdminConflictException: ApplicationException
    {
        public LeastOneAdminConflictException() : base() { }
        public LeastOneAdminConflictException(string message) : base(message) { }

        public LeastOneAdminConflictException(string message, Exception innException) : base(message, innException) { }
    }
}
