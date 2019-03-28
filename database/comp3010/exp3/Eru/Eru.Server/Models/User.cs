using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Interfaces;

namespace Eru.Server.Models
{
    public class User: ITimeEntity
    {
        [Key] [StringLength(32)] public string Id { get; set; }

        [Required] [MaxLength(63)] public string Name { get; set; }

        [Required] [MaxLength(255)] public string Password { get; set; }

        [Required] public string Avatar { get; set; }

        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        [Required]
        public DateTime UpdateTime { get; set; }

        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        [Required]
        public DateTime CreateTime { get; set; }

        [Required] public bool Registered { get; set; }

        [Required] public string Url { get; set; }

        [Required] public string Description { get; set; }
        public HashSet<UserRoleAssociation> UserRoleAssociations { get; set; }
        public List<Post> Posts { get; set; }
        public List<Comment> Comments { get; set; }
        public HashSet<Enrollment> Enrollments { get; set; }

        public UserProfile Profile { get; set; }
    }
}