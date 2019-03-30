using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Interfaces
{
    public interface ICachingProvider
    {
        object Get(string key);
        void Set(string key, object value);
    }
}
