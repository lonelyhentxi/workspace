using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class UserRoleCreateInDto
    {
        [Required]
        public Guid UserId { get; set; }

        [Required]
        public int RoleId { get; set; }
    }
}
