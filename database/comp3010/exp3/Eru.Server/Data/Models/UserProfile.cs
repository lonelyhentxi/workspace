using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Eru.Server.Data.Models
{
    public class UserProfile
    {
        [Key]
        public Guid Id { get; set; }

        [Required]
        public string Misc { get; set; }

        [Required]
        public string Setting { get; set; }

        [ForeignKey("Id")]
        public User User { get; set; }
    }
}
