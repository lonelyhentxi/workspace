using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class PermissionService
    {
        private readonly EruContext _context;
        public PermissionService(EruContext context)
        {
            _context = context;
        }

        public async Task<List<Permission>> Filter(PermissionFilterInDto filterOptions)
        {
            return await (from permission in _context.Permissions
                join association in _context.RolePermissionAssociations
                    on permission.Id equals association.PermissionId
                where (filterOptions.RoleIds == null || filterOptions.RoleIds.Contains(association.RoleId))
                select permission).Distinct().ToListAsync();

        }

        public async Task<List<Permission>> GetAll()
        {
            return await _context.Permissions.ToListAsync();
        }

        public async Task<Permission> Get(int id)
        {
            var permission = await _context.Permissions.FindAsync(id);
            if (permission == null)
            {
                throw new NotExistedException();
            }

            return permission;
        }
    }
}
