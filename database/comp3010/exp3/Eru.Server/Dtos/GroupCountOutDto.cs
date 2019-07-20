using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class GroupCountOutDto<TKey>
    {
        public TKey Id { get; set; }
        public int Count { get; set; }
    }
}
