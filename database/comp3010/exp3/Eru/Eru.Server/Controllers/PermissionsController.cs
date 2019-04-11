using System.Collections.Generic;
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
    public class PermissionsController : ControllerBase
    {
        private readonly PermissionService _permissionService;

        public PermissionsController(PermissionService permissionService)
        {
            _permissionService = permissionService;
        }

        // GET: api/permissions
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<Permission>>>> GetPermissions()
        {
            return Ok(ResultOutDtoBuilder.Success(await _permissionService.GetAll()));
        }

        // GET: api/permissions/5
        [HttpGet("{id}")]
        [ProducesResponseType(200)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<Permission>>> GetPermission(int id)
        {
            try
            {
                var permission = await _permissionService.Get(id);
                return Ok(ResultOutDtoBuilder.Success(permission));
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder.Fail<Permission>(e, "Not exist."));
            }
        }
    }
}