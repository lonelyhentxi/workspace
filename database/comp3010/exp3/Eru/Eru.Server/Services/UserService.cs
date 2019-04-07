using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.Linq;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;
using Eru.Server.Configurations;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Tokens;
using Eru.Server.Exceptions;
using Eru.Server.Services.Interfaces;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.AspNetCore.Identity;
using Newtonsoft.Json.Converters;

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
        private readonly PermissionService _permissionService;

        public UserService(
            EruContext context,
            IUserAvatarService avatarService,
            IUserUrlService urlService,
            IUserProfileService profileService,
            AuthenticationService authenticationService,
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
        }

        public async Task<User> Create(string name, string password)
        {
            #region create_user_if_existed_check 

            var existed = await _context.Users.AnyAsync(u => u.Name == name);
            if (existed)
            {
                return await Task.FromException<User>(new ExistedConflictException());
            }

            #endregion

            #region create_user

            using (var transaction = await _context.Database.BeginTransactionAsync())
            {
                var createTime = new DateTime();
                var newUser = new User
                {
                    Name = name,
                    CreateTime = createTime,
                    UpdateTime = createTime,
                    Password = string.Empty // here has not generated Id, thus cannot generate password
                };
                newUser.Description = GetDefaultDescription(newUser);
                newUser.Avatar = _avatarService.GetDefaultAvatar(newUser);
                newUser.Url = _urlService.GetDefaultUrl(newUser);
                await _context.AddAsync(newUser);
                await _context.SaveChangesAsync();
                newUser.Password = _passwordHasher.HashPassword(newUser.Id.ToString(), password);
                var newUserProfile = new UserProfile
                {
                    Id = newUser.Id,
                    Misc = _profileService.GetDefaultMisc(newUser),
                    Setting = _profileService.GetDefaultSetting(newUser),
                };
                newUser.Profile = newUserProfile;
                await _context.AddAsync(newUserProfile);
                await _context.SaveChangesAsync();
                transaction.Commit();
                return newUser;
            }

            #endregion
        }

        public async Task<dynamic> Login(UserLoginInDto loginParams)
        {
            #region login_user_if_existed_check

            var user = await _context.Users
                .Include(u => u.UserRoleAssociations)
                .ThenInclude(a=>a.Role)
                .SingleAsync(u => u.Name == loginParams.Name);
            if (user == null)
            {
                return await Task.FromException<User>(new NotExistedException());
            }

            #endregion

            #region login_user_password_check

            if (ValidatePassword(user, loginParams.Password))
            {
                return await Task.FromException<User>(new BadAuthenticationException());
            }

            #endregion

            #region login_generate_jwt_bearer_token

            var roles = (from association in user.UserRoleAssociations select association.Role).ToList();
            var permissions = await _permissionService.GetPermissions(new PermissionFilterInDto
            {
                NameMatch = null,
                RoleIds = roles.Select(r=>r.Id).ToList(),
            });
            var claims = new List<Claim>
            {
                new Claim(ClaimTypes.NameIdentifier, user.Id.ToString()),
                new Claim(ClaimTypes.Name,user.Name),
                new Claim(ClaimTypes.Expiration,
                    DateTime.Now.AddSeconds(_authenticationService.ExpirationTime.TotalSeconds).ToString())
            };
            claims.AddRange(roles.Select(r=>new Claim(ClaimTypes.Role,r.Name)));
            claims.AddRange(permissions.Select(p=>new Claim(ClaimTypes.AuthorizationDecision,p.Name)));
            var identity = new ClaimsIdentity(JwtBearerDefaults.AuthenticationScheme);
            var token = _authenticationService.BuildJwtToken(claims);
            return token;

            #endregion
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