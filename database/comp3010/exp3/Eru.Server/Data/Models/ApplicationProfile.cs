using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Eru.Server.Data.Models
{
    public class ApplicationProfile
    {
        [Key]
        public Guid Id { get; set; }

        [Required]
        public string Misc { get; set; }
        public string Setting { get; set; }
        [ForeignKey("Id")]
        public Application Application { get; set; }
    }
}
