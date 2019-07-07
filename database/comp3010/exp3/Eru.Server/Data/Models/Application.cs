using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using Eru.Server.Data.Interfaces;

namespace Eru.Server.Data.Models
{
    public class Application: ITimeEntity
    {
        [Key]
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public Guid Id { get; set; }

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