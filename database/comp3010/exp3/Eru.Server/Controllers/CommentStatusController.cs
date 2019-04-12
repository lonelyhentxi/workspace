using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Eru.Server.Services;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class CommentStatusController : ControllerBase
    {
        private readonly CommentStatusService _commentStatusService;

        public CommentStatusController(CommentStatusService commentStatusService)
        {
            _commentStatusService = commentStatusService;
        }

        // GET: api/commentStatus
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<CommentStatus>>>> GetCommentStatuses()
        {
            return Ok(ResultOutDtoBuilder.Success(await _commentStatusService.GetAll()));
        }
    }
}
