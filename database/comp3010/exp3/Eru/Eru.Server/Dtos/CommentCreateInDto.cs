using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class CommentCreateInDto
    {
        public Guid? ParentId { get; set; }
        [Required]
        public Guid PostId { get; set; }
        [Required]
        public int StatusId { get; set; }
        public int? CategoryId { get; set; }

        [Required] public string Content { get; set; }
    }
}
