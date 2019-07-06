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
    public class UserFilterInDto : ITimeRangeFilterInDto, IPaging
    {
        [DefaultValue(null)] [MaxLength(63)] public string NameMatch { get; set; } = null;
        [DefaultValue(null)] public bool? Registered { get; set; } = null;
        [DefaultValue(null)] public int? RoleId { get; set; } = null;

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