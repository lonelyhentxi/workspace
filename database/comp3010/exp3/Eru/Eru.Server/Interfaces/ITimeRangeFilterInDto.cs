using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Interfaces
{
    public interface ITimeRangeFilterInDto
    {
        DateTime? CreateTimeFrom { get; set; }
        DateTime? CreateTimeTo { get; set; }
        DateTime? UpdateTimeFrom { get; set; }
        DateTime? UpdateTimeTo { get; set; }
    }
}