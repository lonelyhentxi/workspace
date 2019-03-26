using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class Enrollment
    {
        [StringLength(32)] public string UserId { get; set; }
        [StringLength(32)] public string ApplicationId { get; set; }

        [Required] public string Profile { get; set; }

        [Required] public string Setting { get; set; }

        [ForeignKey("UserId")] public User User { get; set; }

        [ForeignKey("ApplicationId")] public Application Application { get; set; }
    }
}