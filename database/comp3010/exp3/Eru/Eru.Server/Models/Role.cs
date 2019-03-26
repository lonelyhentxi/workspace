using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Models
{
    public class Role
    {
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        public int Id { get; set; }

        [Required] [MaxLength(63)] public string Name { get; set; }

        [Required] public string Description { get; set; }
        public HashSet<RolePermissionAssociation> RolePermissionAssociations { get; set; }
        public List<UserRoleAssociation> UserRoleAssociations { get; set; }
    }
}