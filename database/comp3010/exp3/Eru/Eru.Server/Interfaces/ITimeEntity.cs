using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Interfaces
{
    public interface ITimeEntity
    {
        DateTime CreateTime { get; set; }
        DateTime UpdateTime { get; set; }
    }
}
