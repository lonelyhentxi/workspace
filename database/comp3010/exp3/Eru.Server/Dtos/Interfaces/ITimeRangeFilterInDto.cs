using System;

namespace Eru.Server.Dtos.Interfaces
{
    public interface ITimeRangeFilterInDto
    {
        DateTime? CreateTimeFrom { get; set; }
        DateTime? CreateTimeTo { get; set; }
        DateTime? UpdateTimeFrom { get; set; }
        DateTime? UpdateTimeTo { get; set; }
    }
}