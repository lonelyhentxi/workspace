using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Interfaces;

namespace Eru.Server.Models
{
    public class Application: ITimeEntity
    {
        [Key] [StringLength(32)] public string Id { get; set; }

        [Required] [MaxLength(63)] public string Name { get; set; }

        [Required] [MaxLength(63)] public string Version { get; set; }

        [Required] [Url] public string Avatar { get; set; }


        [Required] [Url] public string Url { get; set; }

        [Required] public string Description { get; set; }

        [Required]
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public DateTime CreateTime { get; set; }

        [Required]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public DateTime UpdateTime { get; set; }

        public ApplicationProfile Profile { get; set; }
        public List<Enrollment> Enrollments { get; set; }
    }
}