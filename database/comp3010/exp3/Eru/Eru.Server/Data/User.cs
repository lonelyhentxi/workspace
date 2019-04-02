using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using Eru.Server.Interfaces;
using Newtonsoft.Json;

namespace Eru.Server.Data
{
    public class User : ITimeEntity
    {
        [Key]
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public Guid Id { get; set; }

        [Required] [MaxLength(63)] public string Name { get; set; }

        [Required] [MaxLength(255)] [JsonIgnore] public string Password { get; set; }

        [Required] [Url] public string Avatar { get; set; }

        [Required] public string Token { get; set; }

        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        [Required]
        public DateTime UpdateTime { get; set; }

        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        [Required]
        public DateTime CreateTime { get; set; }

        [Required] public bool Registered { get; set; }

        [Required] [Url] public string Url { get; set; }

        [Required] public string Description { get; set; }
        public HashSet<UserRoleAssociation> UserRoleAssociations { get; set; }

        public List<Post> Posts { get; set; }
        public List<Comment> Comments { get; set; }
        public HashSet<Enrollment> Enrollments { get; set; }

        public UserProfile Profile { get; set; }
    }
}