using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class Post
    {
        [Key] [StringLength(32)] public string ID { get; set; }
        [Required][StringLength(32)] public string UserId { get; set; }
        [Required] public int StatusId { get; set; }
        public int CategoryId { get; set; }
        [Required] public DateTime CreateTime { get; set; }
        [Required] public DateTime UpdateTime { get; set; }
        [Required] public string Title { get; set; }
        [Required] public string Content { get; set; }
        [Required] public string Description { get; set; }

        [ForeignKey("UserId")] public User User { get; set; }
        [ForeignKey("StatusId")] public PostStatus Status { get; set; }
        [ForeignKey("CategoryId")] public PostCategory Category { get; set; }
       

        public HashSet<PostTagAssociation> PostTagAssociations { get; set; }

        public List<Comment> Comments { get; set; }
    }
}