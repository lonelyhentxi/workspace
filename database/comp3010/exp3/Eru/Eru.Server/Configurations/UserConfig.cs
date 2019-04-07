using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Configurations
{
    public class UserConfig
    {
        [Required] public string UrlBase { get; set; }
        [Required] public string AvatarBase { get; set; }
    }
}
