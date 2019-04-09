using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class RoleService
    {
        private readonly EruContext _context;
        private readonly int _adminId;

        public RoleService(EruContext context)
        {
            _context = context;
            _adminId = context.Roles.First(r => r.Name == "admin").Id;
        }
        public int GetAdminId()
        {
            return _adminId;
        }

        public bool HasRole(IEnumerable<UserRoleAssociation> roles,int roleId)
        {
            return roles.Any(r => r.RoleId == roleId);
        }

        public async Task<bool> LeastOneAdminCheck(IEnumerable<UserRoleAssociation> roles)
        {
            var adminRoleId = GetAdminId();
            if (HasRole(roles, adminRoleId))
            {
                var adminNum = await _context
                    .UserRoleAssociations
                    .Where(a => a.RoleId == adminRoleId)
                    .Select(a => a.UserId)
                    .Distinct()
                    .CountAsync();
                if (adminNum <= 1)
                {
                    return false;
                }
            }

            return true;
        }
    } 
}
