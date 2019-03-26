using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class Comment
    {
        [Key] [StringLength(32)] public string Id { get; set; }

        [StringLength(32)] public string ParentId { get; set; }
        [Required] [StringLength(32)] public string PostId { get; set; }
        [Required] [StringLength(32)] public string UserId { get; set; }
        [Required] public int StatusId { get; set; }
        public int TypeId { get; set; }
        [Required] public DateTime CreateTime { get; set; }
        [Required] public DateTime UpdateTime { get; set; }
        [Required] public string Title { get; set; }
        [Required] public string Content { get; set; }
        [Required] public string Description { get; set; }
        [ForeignKey("PostId")] public Post Post { get; set; }
        [ForeignKey("UserId")] public User User { get; set; }
        [ForeignKey("StatusId")] public CommentStatus Status { get; set; }
        [ForeignKey("TypeId")] public CommentType Type { get; set; }
        [ForeignKey("ParentId")] public Comment Parent { get; set; }
        public List<Comment> Children { get; set; }
    }
}