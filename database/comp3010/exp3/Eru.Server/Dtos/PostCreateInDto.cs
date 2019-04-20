using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;

namespace Eru.Server.Dtos
{
    public class PostCreateInDto
    {
        [Required] public int StatusId { get; set; }

        [DefaultValue(null)] public int? CategoryId { get; set; } = null;

        [Required] public List<int> TagIds { get; set; }

        [MaxLength(255)] [Required] public string Title { get; set; }

        [Required] public string Content { get; set; }
        [Required] public string Description { get; set; }

        [Required] public Guid UserId { get; set; }
    }
}