using System;

namespace Eru.Server.Data.Interfaces
{
    public interface ITimeEntity
    {
        DateTime CreateTime { get; set; }
        DateTime UpdateTime { get; set; }
    }
}
