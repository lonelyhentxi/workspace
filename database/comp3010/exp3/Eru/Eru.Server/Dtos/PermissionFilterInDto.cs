using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class PermissionFilterInDto
    {
        public List<int> RoleIds { get; set; }
        [MaxLength(63)]
        public string NameMatch { get; set; }
    }
}
