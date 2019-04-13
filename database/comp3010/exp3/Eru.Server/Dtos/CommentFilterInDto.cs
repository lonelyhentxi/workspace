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
        [DefaultValue(null)] public Guid? ParentId { get; set; } = null;
        public Guid? PostId { get; set; }


        [DefaultValue(null)] public Guid? UserId { get; set; } = null;
        public int? StatusId { get; set; }
        [DefaultValue(null)] public int? CategoryId { get; set; } = null;

        [DefaultValue(null)] public DateTime? CreateTimeFrom { get; set; } = null;
        [DefaultValue(null)] public DateTime? CreateTimeTo { get; set; } = null;
        [DefaultValue(null)] public DateTime? UpdateTimeFrom { get; set; } = null;

        [DefaultValue(null)] public DateTime? UpdateTimeTo { get; set; } = null;

        [DefaultValue(true)] public bool CreateTimeDesc { get; set; } = true;

        [PerPageRange]
        [DefaultValue(10)]
        public int PerPage { get; set; } = 10;
        [PageRange]
        [DefaultValue(1)]
        public int Page { get; set; } = 1;
    }
}