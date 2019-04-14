using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class SessionCreateInDto
    {
        [Required(ErrorMessage = "User name can not be empty.")]
        [MaxLength(63)]
        public string Name { get; set; }

        [Required(ErrorMessage = "User password can not be empty.")]
        [MaxLength(255)]
        public string Password { get; set; }
    }
}
