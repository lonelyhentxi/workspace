using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Eru.Server.Data.Models
{
    public class Permission
    {
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
        [Key]
        public int Id { get; set; }
        [MaxLength(63)]
        [Required]
        public string Name { get; set; }
        [Required]
        public string Description { get; set; }
        public HashSet<RolePermissionAssociation> RolePermissionAssociations { get; set; }
    }
}
