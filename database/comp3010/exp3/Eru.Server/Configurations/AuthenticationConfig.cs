using System.ComponentModel.DataAnnotations;

namespace Eru.Server.Configurations
{
    public class AuthenticationConfig
    {
        [Required]
        public string Secret { get; set; }
        [Required]
        public string Issuer { get; set; }
        [Required]
        public string Audience { get; set; }
        [Required]
        [Range(0.0,365.0)]
        public double ExpiresDays { get; set; }
    }
}
