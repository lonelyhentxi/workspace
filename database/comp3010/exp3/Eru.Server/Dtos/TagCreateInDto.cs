using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class TagCreateInDto
    {
        [Required]
        [MaxLength(63)]
        public string Name { get; set; }

        [Required]
        public string Description { get; set; }
    }
}
