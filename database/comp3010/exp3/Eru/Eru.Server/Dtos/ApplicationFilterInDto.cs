using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Annotations;
using Eru.Server.Interfaces;

namespace Eru.Server.Dtos
{
    public class ApplicationFilterInDto : ITimeRangeFilterInDto, IPaging
    {
        [MaxLength(63)] public string NameMatch { get; set; }
        public DateTime? CreateTimeFrom { get; set; }
        public DateTime? CreateTimeTo { get; set; }
        public DateTime? UpdateTimeFrom { get; set; }
        public DateTime? UpdateTimeTo { get; set; }

        [DefaultValue(true)]
        public bool CreateTimeDesc { get; set; }
        [DefaultValue(1)] [PageRange] public int Page { get; set; }
        [DefaultValue(10)] [PerPageRange] public int PerPage { get; set; }
    }
}