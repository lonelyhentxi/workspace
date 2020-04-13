using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Eru.Server.Data.Models
{
    public class PostCategory
    {
        [Key]
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public int Id { get; set; }

        [Required] [MaxLength(63)] public string Name { get; set; }
        [Required] public string Description { get; set; }
        public List<Post> Posts { get; set; }
    }
}