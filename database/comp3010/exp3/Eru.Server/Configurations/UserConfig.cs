using System.ComponentModel.DataAnnotations;

namespace Eru.Server.Configurations
{
    public class UserConfig
    {
        [Required] public string UrlBase { get; set; }
        [Required] public string AvatarBase { get; set; }
    }
}
