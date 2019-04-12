using System;
using System.ComponentModel.DataAnnotations.Schema;

namespace Eru.Server.Data.Models
{
    public class PostTagAssociation
    {
        public int TagId { get; set; }
        public Guid PostId { get; set; }
        [ForeignKey("PostId")] public Post Post { get; set; }
        [ForeignKey("TagId")] public PostTag Tag { get; set; }
    }
}