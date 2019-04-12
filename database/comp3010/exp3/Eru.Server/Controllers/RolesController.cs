using System;
using System.Collections.Generic;
using System.Linq;
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
    public class RolesController : ControllerBase
    {
        private readonly RoleService _roleService;
        public RolesController(RoleService roleService)
        {
            _roleService = roleService;
        }

        // GET: api/roles
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<Role>>>> GetRoles()
        {
            return Ok(ResultOutDtoBuilder.Success(await _roleService.GetAll()));
        }

        // GET: api/roles/5
        [HttpGet("{id}")]
        [ProducesResponseType(200)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<Role>>> GetRole(int id)
        {
            try
            {
                var role = await _roleService.Get(id);
                return Ok(ResultOutDtoBuilder.Success(role));
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder.Fail<Role>(e, "Not exist."));
            }
        }
    }
}
