using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class UserCreateInDto
    {
        [MaxLength(63)] [Required] public string Name { get; set; }
        [MaxLength(255)] [Required] public string Password { get; set; }
    }
}