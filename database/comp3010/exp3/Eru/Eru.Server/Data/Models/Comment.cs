using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using Eru.Server.Data.Interfaces;

namespace Eru.Server.Data.Models
{
    public class Comment: ITimeEntity
    {
        [Key]
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public Guid Id { get; set; }

        public Guid? ParentId { get; set; }
        [Required] public Guid PostId { get; set; }
        [Required] public Guid UserId { get; set; }
        [Required] public int StatusId { get; set; }
        public int? CategoryId { get; set; }

        [Required]
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public DateTime CreateTime { get; set; }

        [Required]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public DateTime UpdateTime { get; set; }

        [Required] public string Content { get; set; }
        [ForeignKey("PostId")] public Post Post { get; set; }
        [ForeignKey("UserId")] public User User { get; set; }
        [ForeignKey("StatusId")] public CommentStatus Status { get; set; }
        [ForeignKey("CategoryId")] public CommentCategory Category { get; set; }
        [ForeignKey("ParentId")] public Comment Parent { get; set; }
        public List<Comment> Children { get; set; }
    }
}