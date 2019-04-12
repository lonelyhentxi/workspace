using System;
using System.Threading.Tasks;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Eru.Server.Services;
using Microsoft.AspNetCore.Mvc;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class UserRoleController: ControllerBase
    {
        private readonly RoleService _roleService;

        public UserRoleController(RoleService roleService)
        {
            _roleService = roleService;
        }

        [HttpPost]
        [ProducesResponseType(200)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<UserRoleAssociation>>> PostUserRole([FromBody] UserRoleCreateInDto options)
        {
            try
            {
                var res = await _roleService.AddRolePlayer(options.UserId, options.RoleId);
                return Ok(ResultOutDtoBuilder.Success(res));
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder
                    .Fail<UserRoleAssociation>(e, "Not exist."));
            }
        }

        [HttpDelete("{userId}:{roleId}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        [ProducesResponseType(409)]
        public async Task<ActionResult<ResultOutDto<object>>> DeleteUserRole([FromRoute] string userId,[FromRoute] int roleId)
        {
            if (Guid.TryParse(userId, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<object>(new FormatException(), "Error user id format."));
            }

            try
            {
                await _roleService.RemoveRolePlayer(guid, roleId);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder
                    .Fail<object>(e, "Not exist."));
            }
            catch (LeastOneAdminConflictException e)
            {
                return Conflict(ResultOutDtoBuilder
                    .Fail<object>(e, "At least one admin."));
            }
        }
    }
}
