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
        [StringLength(32)] public string ParentId { get; set; }
        [StringLength(32)] public string PostId { get; set; }
        [StringLength(32)] public string UserId { get; set; }
        public int? StatusId { get; set; }
        public int? CategoryId { get; set; }

        public DateTime? CreateTimeFrom { get; set; }
        public DateTime? CreateTimeTo { get; set; }
        public DateTime? UpdateTimeFrom { get; set; }
        public DateTime? UpdateTimeTo { get; set; }

        [DefaultValue(true)]
        public bool CreateTimeDesc { get; set; }

        [PageRange] [DefaultValue(1)] public int Page { get; set; }
        [PerPageRange] [DefaultValue(20)] public int PerPage { get; set; }
    }
}