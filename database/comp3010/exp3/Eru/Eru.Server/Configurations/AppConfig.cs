using System.ComponentModel.DataAnnotations;

namespace Eru.Server.Configurations
{
    public class AppConfig
    {
        public int? Port { get; set; }

        [Required]
        [RegularExpression(@"^http|https$",
            ErrorMessage = "Unknown protocol.")]
        public string Protocol { get; set; }

        [Required]
        public string Domain { get; set; }
    }
}