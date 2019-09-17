using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Claims;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Data.Utils;
using Eru.Server.Dtos;
using Microsoft.EntityFrameworkCore;
using Eru.Server.Exceptions;
using Eru.Server.Services.Interfaces;
using Microsoft.AspNetCore.Identity;

namespace Eru.Server.Services
{
    public class UserService
    {
        private readonly EruContext _context;
        private readonly PasswordHasher<string> _passwordHasher;
        private readonly IUserAvatarService _avatarService;
        private readonly IUserUrlService _urlService;
        private readonly IUserProfileService _profileService;
        private readonly AuthenticationService _authenticationService;
        private readonly RoleService _roleService;
        private readonly PermissionService _permissionService;

        public UserService(
            EruContext context,
            IUserAvatarService avatarService,
            IUserUrlService urlService,
            IUserProfileService profileService,
            AuthenticationService authenticationService,
            RoleService roleService,
            PermissionService permissionService
        )
        {
            _context = context;
            _passwordHasher = new PasswordHasher<string>();
            _avatarService = avatarService;
            _urlService = urlService;
            _profileService = profileService;
            _authenticationService = authenticationService;
            _permissionService = permissionService;
            _roleService = roleService;
        }

        public async Task<User> Create(UserCreateInDto createParams)
        {
            #region create_user_if_existed_check 

            var existed = await _context.Users.AnyAsync(u => u.Name == createParams.Name);
            if (existed)
            {
                return await Task.FromException<User>(new ExistedConflictException());
            }

            #endregion

            #region create_user

            using (var transaction = await _context.Database.BeginTransactionAsync())
            {
                var createTime = DateTime.Now;
                var newUser = new User
                {
                    Name = createParams.Name,
                    CreateTime = createTime,
                    UpdateTime = createTime,
                    Password = string.Empty // here has not generated Id, thus cannot generate password
                };
                newUser.Description = GetDefaultDescription(newUser);
                newUser.Avatar = _avatarService.GetDefaultAvatar(newUser);
                newUser.Url = _urlService.GetDefaultUrl(newUser);
                await _context.AddAsync(newUser);
                await _context.SaveChangesAsync();
                newUser.Password = _passwordHasher.HashPassword(newUser.Id.ToString(), createParams.Password);
                var newUserProfile = new UserProfile
                {
                    Id = newUser.Id,
                    Misc = _profileService.GetDefaultMisc(newUser),
                    Setting = _profileService.GetDefaultSetting(newUser),
                };
                newUser.Profile = newUserProfile;
                await _context.AddAsync(newUserProfile);
                await _context.SaveChangesAsync();
                await _roleService.AddRolePlayerWithoutCheck(newUser.Id, _roleService.GetDefaultId());
                transaction.Commit();
                return newUser;
            }

            #endregion
        }

        public async Task<SessionOutDto> Login(SessionCreateInDto loginParams)
        {
            #region login_user_if_existed_check

            var user = await _context.Users
                .Include(u => u.UserRoleAssociations)
                .ThenInclude(a => a.Role)
                .FirstOrDefaultAsync(u => u.Name == loginParams.Name);
            if (user == null)
            {
                throw new NotExistedException();
            }

            #endregion

            #region login_user_password_check

            if (!ValidatePassword(user, loginParams.Password))
            {
                throw new BadAuthenticationException();
            }

            #endregion

            #region login_generate_jwt_bearer_token

            var roles = (from association in user.UserRoleAssociations select association.Role).ToList();
            var permissions = await _permissionService.Filter(new PermissionFilterInDto
            {
                RoleIds = roles.Select(r => r.Id).ToList(),
            });
            var claims = new List<Claim>
            {
                new Claim(ClaimTypes.NameIdentifier, user.Id.ToString()),
                new Claim(ClaimTypes.Name, user.Name),
                new Claim(ClaimTypes.Expiration,
                    DateTime.Now.AddSeconds(_authenticationService.ExpirationTime.TotalSeconds).ToString())
            };
            claims.AddRange(roles.Select(r => new Claim(ClaimTypes.Role, r.Name)));
            claims.AddRange(permissions.Select(p => new Claim(ClaimTypes.AuthorizationDecision, p.Name)));
            var token = _authenticationService.BuildJwtToken(claims);


            #endregion

            #region update_last_activate_info

            _context.Entry(user).State = EntityState.Modified;
            user.UpdateTime = DateTime.Now;
            await _context.SaveChangesAsync();

            #endregion

            return new SessionOutDto {User = user, Token = token};
        }

        public async Task<List<User>> Filter(UserFilterInDto filterOptions)
        {
            var filtered = _context.Users
                .Include(u => u.UserRoleAssociations)
                .Where(u => (
                    (filterOptions.Registered == null)
                    && (string.IsNullOrWhiteSpace(filterOptions.NameMatch) || u.Name.Contains(filterOptions.NameMatch))
                    && (filterOptions.RoleId == null ||
                        u.UserRoleAssociations.Any(r => r.RoleId == filterOptions.RoleId))
                ))
                .TimeRangeFilter(filterOptions);

            var ordered = filterOptions.CreateTimeDesc
                ? filtered.OrderByDescending(u => u.CreateTime)
                : filtered.OrderBy(u => u.CreateTime);
            return await ordered.SkipTakePaging(filterOptions).ToListAsync();
        }

        public async Task<User> Get(Guid id,bool detailed)
        {
            var user = await (_context.Users
                .Include(u => u.UserRoleAssociations)
                .FirstOrDefaultAsync(u => u.Id == id));
            if (user == null)
            {
                return await Task.FromException<User>(new NotExistedException());
            }

            if (!detailed)
            {
                return user;
            }

            var profile = 
               await (from p in _context.UserProfiles
                    where p.Id == user.Id
                    select p)
                .FirstOrDefaultAsync();
            user.Profile = profile;
            return user;

        }

        public async Task<User> Update(User user)
        {
            if (await _context.Users.AnyAsync(u => (u.Name == user.Name && u.Id != user.Id)))
            {
                throw new ExistedConflictException();
            }
            _context.Entry(user).State = EntityState.Modified;
            _context.Entry(user).Property(u => u.Password).IsModified = false;
            if (user.Profile != null)
            {
                _context.Entry(user.Profile).State = EntityState.Modified;
            }
            if (string.IsNullOrWhiteSpace(user.Password))
            {
                user.Password = _passwordHasher.HashPassword(user.Id.ToString(), user.Password);
                _context.Entry(user).Property(u => u.Password).IsModified = true;
            }
            await _context.SaveChangesAsync();
            return user;
        }
        public async Task<User> Remove(Guid id)
        {
            // may throw NotExistedException
            var user = await Get(id, false);

            var safety = await _roleService.LeastOneAdminCheck(user.UserRoleAssociations);
            if (!safety)
            {
                return await Task.FromException<User>(new LeastOneAdminConflictException());
            }
            _context.Users.Remove(user);
            await _context.SaveChangesAsync();
            return user;
        }

        public string GetDefaultDescription(User _)
        {
            return string.Empty;
        }

        public bool ValidatePassword(User user, string password)
        {
            return _passwordHasher.VerifyHashedPassword(user.Id.ToString(), user.Password, password) ==
                   PasswordVerificationResult.Success;
        }
    }
}