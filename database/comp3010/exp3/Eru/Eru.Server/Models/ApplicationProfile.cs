using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class ApplicationProfile
    { 
        [Key]
        [StringLength(32)]
        public string Id { get; set; }
        public string Misc { get; set; }
        public string Setting { get; set; }
        [ForeignKey("Id")]
        public Application Application { get; set; }
    }
}
