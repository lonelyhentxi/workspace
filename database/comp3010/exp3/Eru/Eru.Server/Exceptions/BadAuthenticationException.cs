using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Exceptions
{
    public class BadAuthenticationException : ApplicationException
    {
        public BadAuthenticationException() : base()
        {
        }

        public BadAuthenticationException(string message) : base(message)
        {
        }

        public BadAuthenticationException(string message, Exception inner) : base(message, inner)
        {
        }
    }
}