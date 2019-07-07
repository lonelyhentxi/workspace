using System.ComponentModel.DataAnnotations.Schema;

namespace Eru.Server.Data.Models
{
    public class RolePermissionAssociation
    {
        public int RoleId { get; set; }
        public int PermissionId { get; set; }
        [ForeignKey("RoleId")]
        public Role Role { get; set; }
        [ForeignKey("PermissionId")]
        public Permission Permission { get; set; }
    }
}
