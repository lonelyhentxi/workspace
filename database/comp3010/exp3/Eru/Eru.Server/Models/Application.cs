using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class Application
    {
        [Key]
        [StringLength(32)]
        public string Id { get; set; }

        [Required]
        [MaxLength(63)]
        public string Name { get; set; }

        [Required]
        public string Avatar { get; set; }
        [Required]
        public string Url { get; set; }
        [Required]
        public string Description { get; set; }
        [Required]
        public DateTime CreateTime { get; set; }
        [Required]
        public DateTime LastActiveTime { get; set; }
        public ApplicationProfile Profile { get; set; }
        public List<Enrollment> Enrollments { get; set; } 
    }
}
