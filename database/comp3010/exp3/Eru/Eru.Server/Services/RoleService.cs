using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Exceptions;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class RoleService
    {
        private readonly EruContext _context;
        private readonly int _adminId;
        private readonly int _defaultId;

        public RoleService(EruContext context)
        {
            _context = context;
            _adminId = context.Roles.First(r => r.Name == "admin").Id;
            _defaultId = context.Roles.First(r => r.Name == "user").Id;
        }

        public async Task<List<Role>> GetAll()
        {
            return await _context.Roles.Include(r => r.RolePermissionAssociations).ToListAsync();
        }

        public async Task<Role> Get(int id)
        {
            var role = await _context.Roles
                .Include(r => r.RolePermissionAssociations)
                .Include(r => r.UserRoleAssociations)
                .FirstAsync(r => r.Id == id);
            if (role == null)
            {
                throw new NotExistedException();
            }

            return role;
        }

        public async Task<UserRoleAssociation> AddRolePlayerWithoutCheck(Guid userId, int roleId)
        {
            var newUserRoleAssociation = new UserRoleAssociation
            {
                RoleId = roleId,
                UserId = userId,
            };
            await _context.UserRoleAssociations.AddAsync(newUserRoleAssociation);
            await _context.SaveChangesAsync();
            return newUserRoleAssociation;
        }

        public async Task<UserRoleAssociation> AddRolePlayer(Guid userId, int roleId)
        {
            var existences = await Task.WhenAll(new Task<bool>[]
            {
                _context.Users.AnyAsync(u => u.Id == userId),
                _context.Roles.AnyAsync(r => r.Id == roleId)
            });
            if (existences.Any(e => !e))
            {
                throw new NotExistedException();
            }

            return await AddRolePlayerWithoutCheck(userId, roleId);
        }

        public async Task<UserRoleAssociation> RemoveRolePlayer(Guid userId, int roleId)
        {
            var userRoleAssociation = await _context.UserRoleAssociations
                .FirstAsync(a => a.UserId == userId && a.RoleId == roleId);
            if (userRoleAssociation == null)
            {
                throw new NotExistedException();
            }

            if (roleId == GetAdminId()&&!await LeastOneAdminCheck())
            {
                throw new LeastOneAdminConflictException();
            }

            _context.UserRoleAssociations.Remove(userRoleAssociation);
            await _context.SaveChangesAsync();
            return userRoleAssociation;

        }

        public int GetDefaultId()
        {
            return _defaultId;
        }

        public int GetAdminId()
        {
            return _adminId;
        }

        public bool HasRole(IEnumerable<UserRoleAssociation> roles, int roleId)
        {
            return roles.Any(r => r.RoleId == roleId);
        }

        public async Task<bool> LeastOneAdminCheck(IEnumerable<UserRoleAssociation> roles)
        {
            var adminRoleId = GetAdminId();
            if (HasRole(roles, adminRoleId))
            {
                return await LeastOneAdminCheck();
            }

            return true;
        }

        public async Task<bool> LeastOneAdminCheck()
        {
            var adminRoleId = GetAdminId();
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

            return true;
        }
    }
}