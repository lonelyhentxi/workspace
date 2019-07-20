using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using Eru.Server.Data.Interfaces;

namespace Eru.Server.Data.Models
{
    public class Post: ITimeEntity
    {
        [Key]
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public Guid Id { get; set; }
        [Required] public Guid UserId { get; set; }
        [Required] public int StatusId { get; set; }
        public int? CategoryId { get; set; }

        [Required]
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public DateTime CreateTime { get; set; }

        [Required]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public DateTime UpdateTime { get; set; }

        [Required] [MaxLength(255)] public string Title { get; set; }
        [Required] public string Content { get; set; }
        [Required] public string Description { get; set; }

        [ForeignKey("UserId")] public User User { get; set; }
        [ForeignKey("StatusId")] public PostStatus Status { get; set; }
        [ForeignKey("CategoryId")] public PostCategory Category { get; set; }


        public HashSet<PostTagAssociation> PostTagAssociations { get; set; }

        public List<Comment> Comments { get; set; }
    }
}