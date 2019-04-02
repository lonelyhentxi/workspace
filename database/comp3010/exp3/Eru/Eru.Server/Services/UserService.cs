using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.Linq;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Utils;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Tokens;
using Eru.Server.Exceptions;
using Microsoft.AspNetCore.Identity;

namespace Eru.Server.Services
{
    public class UserService
    {
        private readonly AppSettings _appSettings;
        private readonly EruContext _context;
        private readonly PasswordHasher<string> _passwordHasher;

        public UserService(IOptions<AppSettings> appSettings, EruContext context)
        {
            _appSettings = appSettings.Value;
            _context = context;
            _passwordHasher = new PasswordHasher<string>();
        }

        public async Task<User> Create(string name, string password)
        {
            var existed = await _context.Users.AnyAsync(u => u.Name == name);
            if (existed)
            {
                return await Task.FromException<User>(new ExistedConflictException());
            }

            var passwordHasher = new PasswordHasher<string>();
            var newUser = new User
            {
                Password = password.
            };
            await _context.Users.AddAsync(new User
            {
                
            });
        }

        public async Task<User> Authenticate(string name, string password)
        {
            var user = await _context.Users
                .Include(u => u.UserRoleAssociations)
                .FirstAsync(u => u.Name == name && u.Password == password);
            if (user == null)
            {
                return null;
            }

            var tokenHandler = new JwtSecurityTokenHandler();
            var key = Encoding.ASCII.GetBytes(_appSettings.Secret);
            var tokenDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(new Claim[]
                {
                    new Claim(ClaimTypes.Name, user.Id.ToString()),
                }),
                Expires = DateTime.UtcNow.AddDays(7),
                SigningCredentials = new SigningCredentials(new SymmetricSecurityKey(key),
                    SecurityAlgorithms.HmacSha256Signature)
            };
            var token = tokenHandler.CreateToken(tokenDescriptor);
            user.Token = tokenHandler.WriteToken(token);
            user.Password = null;
            return user;
        }
    }
}