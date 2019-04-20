using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Dtos
{
    public class CommentCreateInDto
    {
        [DefaultValue(null)] public Guid? ParentId { get; set; } = null;
        [Required]
        public Guid PostId { get; set; }
        [Required]
        public int StatusId { get; set; }

        [DefaultValue(null)] public int? CategoryId { get; set; } = null;
         
        [Required] public string Content { get; set; }

        [Required] public Guid UserId { get; set; }
    }
}
