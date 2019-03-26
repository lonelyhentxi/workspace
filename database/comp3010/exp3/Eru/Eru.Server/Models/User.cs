using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class User
    {
        [Key] [StringLength(32)] public string ID { get; set; }

        [Required] [MaxLength(63)] public string Name { get; set; }

        [Required] [MaxLength(255)] public string Password { get; set; }

        [Required] public string Avatar { get; set; }

        [Required] public DateTime LastActiveTime { get; set; }

        [Required] public DateTime JoinedTime { get; set; }

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