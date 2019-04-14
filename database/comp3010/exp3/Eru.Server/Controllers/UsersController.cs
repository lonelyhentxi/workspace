using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Microsoft.AspNetCore.Mvc;
using Eru.Server.Services;

namespace Eru.Server.Controllers
{
    

    [Route("api/[controller]")]
    [ApiController]
    public class UsersController : ControllerBase
    {
        private readonly UserService _userService;

        public UsersController(UserService userService)
        {
            _userService = userService;
        }

        // GET: api/users
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<User>>>> GetUsers([FromQuery] UserFilterInDto filterOptions)
        {
            var filteredUsers = await _userService.Filter(filterOptions);
            return Ok(ResultOutDtoBuilder.Success(filteredUsers));
        }


        // POST: api/users
        [HttpPost]
        [ProducesResponseType(200)]
        [ProducesResponseType(409)]
        public async Task<ActionResult<ResultOutDto<User>>> PostUser([FromBody] UserCreateInDto createOptions)
        {
            try
            {
                var user = await _userService.Create(createOptions);
                return Ok(ResultOutDtoBuilder.Success(user));
            }
            catch (ExistedConflictException e)
            {
                return Conflict(ResultOutDtoBuilder.Fail<User>(e, "New name conflict with other existed user."));
            }
        }


        // GET: api/users/5
        [HttpGet("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<User>>> GetUser(string id)
        {
            if (!Guid.TryParse(id, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder.Fail<User>(new FormatException(), "Error id format"));
            }
            try
            {
                var user = await _userService.Get(guid, true);
                return Ok(ResultOutDtoBuilder.Success(user));
            }
            catch (NotExistedException e)
            {
                return NotFound(
                    ResultOutDtoBuilder.Fail<User>(
                        e, "Not found the user."));
            }
        }

        // PUT: api/users/5
        [HttpPut("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        [ProducesResponseType(409)]
        public async Task<ActionResult<ResultOutDto<User>>> PutUser(string id, User user)
        {
            if (Guid.TryParse(id,out Guid guid)|| guid!=user.Id)
            {
                return BadRequest(ResultOutDtoBuilder.Fail<User>(new FormatException(), "Error id format"));
            }

            try
            {
                await _userService.Update(user);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder.Fail<User>(e,"Not exist."));
            }
            catch (ExistedConflictException e)
            {
                return Conflict(ResultOutDtoBuilder.Fail<User>(e, "New name conflict with other existed user."));
            }
        }

        // DELETE: api/users/1
        [HttpDelete("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        [ProducesResponseType(409)]
        public async Task<ActionResult<ResultOutDto<User>>> DeleteUser(string id)
        {
            if (!Guid.TryParse(id, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder.Fail<User>(new FormatException(), "Error id format"));
            }
            try
            {
                await _userService.Remove(guid);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(
                    ResultOutDtoBuilder.Fail<User>(
                        e, "Not found the user."));
            }
            catch (LeastOneAdminConflictException e)
            {
                return Conflict(
                    ResultOutDtoBuilder.Fail<User>(
                        e, "Can not delete, should at lease have one admin."
                    ));
            }
        }
    }
}
