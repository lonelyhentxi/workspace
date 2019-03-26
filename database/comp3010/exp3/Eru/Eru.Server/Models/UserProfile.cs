using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class UserProfile
    {
        [Key]
        [StringLength(32)]
        public string Id { get; set; }

        [Required]
        public string Misc { get; set; }

        [Required]
        public string Setting { get; set; }

        [ForeignKey("Id")]
        public User User { get; set; }
    }
}
