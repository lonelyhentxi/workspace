using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Annotations;
using Eru.Server.Dtos.Interfaces;

namespace Eru.Server.Dtos
{
    public class CommentFilterInDto : ITimeRangeFilterInDto, IPaging
    {
        public Guid? ParentId { get; set; }
        public Guid? PostId { get; set; }
        public Guid? UserId { get; set; }
        public int? StatusId { get; set; }
        public int? CategoryId { get; set; }

        public DateTime? CreateTimeFrom { get; set; }
        public DateTime? CreateTimeTo { get; set; }
        public DateTime? UpdateTimeFrom { get; set; }
        public DateTime? UpdateTimeTo { get; set; }

        [DefaultValue(true)]
        public bool CreateTimeDesc { get; set; }

        [PerPageRange]
        public int PerPage { get; set; } = 10;
        [PageRange]
        public int Page { get; set; } = 1;
    }
}