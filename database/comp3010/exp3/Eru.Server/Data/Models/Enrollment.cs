using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Eru.Server.Data.Models
{
    public class Enrollment
    {
        public Guid UserId { get; set; }
        public Guid ApplicationId { get; set; }

        [Required] public string Profile { get; set; }

        [Required] public string Setting { get; set; }

        [ForeignKey("UserId")] public User User { get; set; }

        [ForeignKey("ApplicationId")] public Application Application { get; set; }
    }
}