using System;
using System.ComponentModel.DataAnnotations.Schema;

namespace Eru.Server.Data.Models
{
    public class UserRoleAssociation
    {
        public Guid UserId { get; set; }
        public int RoleId { get; set; }
        [ForeignKey("UserId")] public User User { get; set; }
        [ForeignKey("RoleId")] public Role Role { get; set; }
    }
}