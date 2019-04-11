using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Eru.Server.Services;
using Microsoft.AspNetCore.Mvc;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class SessionsController : ControllerBase
    {
        private readonly EruContext _context;
        private readonly UserService _userService;

        public SessionsController(EruContext context, UserService userService)
        {
            _context = context;
            _userService = userService;
        }

        [HttpPost]
        [ProducesResponseType(200)]
        [ProducesResponseType(404)]
        [ProducesResponseType(401)]
        public async Task<ActionResult<ResultOutDto<object>>> CreateSession([FromBody] SessionCreateInDto createOptions)
        {
            try
            {
                var token = await _userService.Login(createOptions);
                return Ok(ResultOutDtoBuilder.Success(token));
            }
            catch (NotExistedException e)
            {
                return NotFound(
                    ResultOutDtoBuilder
                        .Fail<object>(e, "Target user not exist.")
                );
            }
            catch (BadAuthenticationException e)
            {
                return Unauthorized(
                    ResultOutDtoBuilder
                        .Fail<object>(e, "User name and password don't match.")
                );
            }
        }
    }
}