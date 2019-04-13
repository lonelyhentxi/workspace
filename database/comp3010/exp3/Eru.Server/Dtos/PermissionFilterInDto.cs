using System.Collections.Generic;
using System.ComponentModel;

namespace Eru.Server.Dtos
{
    public class PermissionFilterInDto
    {
        [DefaultValue(null)] public List<int> RoleIds { get; set; } = null;
    }
}
