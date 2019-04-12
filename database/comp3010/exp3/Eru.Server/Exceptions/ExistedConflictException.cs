using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Exceptions
{
    public class ExistedConflictException : ApplicationException
    {
        public ExistedConflictException() : base() { }
        public ExistedConflictException(string message) : base(message) { }
        public ExistedConflictException(string message, Exception inner) : base(message, inner) { }
    }
}
