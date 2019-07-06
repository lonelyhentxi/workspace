using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class ApplicationCreateInDto
    {
        [Required] [MaxLength(63)] public string Name { get; set; }

        [Required] [MaxLength(63)] public string Version { get; set; }

        [Required] [Url] public string Avatar { get; set; }


        [Required] [Url] public string Url { get; set; }

        [Required] public string Description { get; set; }
    }
}
