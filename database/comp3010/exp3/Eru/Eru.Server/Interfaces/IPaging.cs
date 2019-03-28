using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Interfaces
{
    public interface IPaging
    {
        int Page { get; set; }


        int PerPage { get; set; }
    }
}