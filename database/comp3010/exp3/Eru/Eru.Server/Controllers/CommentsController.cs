using System;
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
    public class CommentsController : ControllerBase
    {
        private readonly CommentService _commentService;

        public CommentsController(CommentService commentService)
        {
            _commentService = commentService;
        }

        // GET: api/Comments
        [HttpGet]
        public async Task<ActionResult<ResultOutDto<IEnumerable<Comment>>>> GetComments([FromQuery] CommentFilterInDto filterOptions)
        {
            return Ok(ResultOutDtoBuilder
                .Success(await _commentService.Filter(filterOptions)));
        }

        // GET: api/comments/5
        [HttpGet("{id}")]
        public async Task<ActionResult<Comment>> GetComment(string id)
        {
            if (Guid.TryParse(id, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<Comment>(new FormatException(), "Error guid format."));
            }

            try
            {
                return Ok(ResultOutDtoBuilder
                    .Success(await _commentService.Get(guid)));
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder
                    .Fail<Comment>(e, "Not exist."));
            }
        }

        // PUT: api/comments/5
        [HttpPut("{id}")]
        public async Task<ActionResult<ResultOutDto<object>>> PutComment(string id,Comment comment)
        {
            if (Guid.TryParse(id, out Guid guid)||guid!=comment.Id)
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<Comment>(new FormatException(), "Error guid format."));
            }

            try
            {
                await _commentService.Update(comment);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder
                    .Fail<Comment>(e, "Not exist."));
            }
        }

        // POST: api/comments
        [HttpPost]
        public async Task<ActionResult<ResultOutDto<Comment>>> PostComment(CommentCreateInDto createOptions)
        {
           // TODO: add auth and fix here
           var user = new User();
           var comment = await _commentService.Create(createOptions, user);
           return Ok(ResultOutDtoBuilder.Success(comment));
        }

        // DELETE: api/Comments/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<ResultOutDto<object>>> DeleteComment(string id)
        {
            if (Guid.TryParse(id, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<Comment>(new FormatException(), "Error guid format."));
            }
            try
            {
                await _commentService.Remove(guid);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder
                    .Fail<Comment>(e, "Not exist."));
            }
        }
    }
}