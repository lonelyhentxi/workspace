using System.Collections.Generic;
using System.Threading.Tasks;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Eru.Server.Services;
using Microsoft.AspNetCore.Mvc;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class PostStatusController : ControllerBase
    {
        private readonly PostStatusService _statusService;

        public PostStatusController(PostStatusService statusService)
        {
            _statusService = statusService;
        }

        // GET: api/postStatus
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<PostStatus>>>> GetPostStatuses()
        {
            return Ok(ResultOutDtoBuilder.Success(await _statusService.GetAll()));
        }
    }
}
