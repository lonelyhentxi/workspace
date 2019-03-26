using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class UserRoleAssociation
    {
        [StringLength(32)] public string UserId { get; set; }
        public int RoleId { get; set; }
        [ForeignKey("UserId")] public User User { get; set; }
        [ForeignKey("RoleId")] public Role Role { get; set; }
    }
}