using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Data.Utils;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using Eru.Server.Services;
using Microsoft.AspNetCore.Authorization;

namespace Eru.Server.Controllers
{
    

    [Route("api/[controller]")]
    [ApiController]
    public class UsersController : ControllerBase
    {
        private readonly EruContext _context;
        private readonly UserService _userService;

        public UsersController(EruContext context, UserService userService)
        {
            _context = context;
            _userService = userService;
        }

        // GET: api/Users
        [HttpGet]
        public async Task<ActionResult<IEnumerable<User>>> GetUsers([FromQuery] UserFilterInDto filterOptions)
        {
            var mid = _context.Users
                .Include(u => u.UserRoleAssociations)
                .Where(u => (
                    (filterOptions.Registered == null)
                    && (string.IsNullOrWhiteSpace(filterOptions.NameMatch) || u.Name.Contains(filterOptions.NameMatch))
                    && (filterOptions.RoleId == null ||
                        u.UserRoleAssociations.Any(r => r.RoleId == filterOptions.RoleId))
                ))
                .TimeRangeFilter(filterOptions);
            if (filterOptions.CreateTimeDesc)
            {
                return await mid.OrderByDescending(u => u.CreateTime).SkipTakePaging(filterOptions).ToListAsync();
            }

            return await mid.OrderBy(u => u.CreateTime).SkipTakePaging(filterOptions).ToListAsync();
        }

        // GET: api/Users/5
        [HttpGet("{id}")]
        public async Task<ActionResult<User>> GetUser(string id)
        {
            var user = await _context.Users
                .Include(u=>u.Profile)
                .FirstAsync(u=>u.Id.ToString()==id);

            if (user == null)
            {
                return NotFound();
            }

            return user;
        }

        // PUT: api/Users/5
        [HttpPut("{id}")]
        public async Task<IActionResult> PutUser(string id, User user)
        {
            if (id != user.Id.ToString())
            {
                return BadRequest();
            }

            _context.Entry(user).State = EntityState.Modified;

            try
            {
                await _context.SaveChangesAsync();
            }
            catch (DbUpdateConcurrencyException)
            {
                if (!UserExists(id))
                {
                    return NotFound();
                }
                else
                {
                    throw;
                }
            }

            return NoContent();
        }

        // POST: api/Users
        [HttpPost]
        public async Task<ActionResult<User>> PostUser(User user)
        {
            _context.Users.Add(user);
            await _context.SaveChangesAsync();

            return CreatedAtAction("GetUser", new { id = user.Id }, user);
        }

        // DELETE: api/Users/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<User>> DeleteUser(string id)
        {
            var user = await _context.Users.FindAsync(id);
            if (user == null)
            {
                return NotFound();
            }

            _context.Users.Remove(user);
            await _context.SaveChangesAsync();

            return user;
        }

        private bool UserExists(string id)
        {
            return _context.Users.Any(e => e.Id.ToString() == id);
        }
    }
}
