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
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<Comment>>>> GetComments([FromQuery] CommentFilterInDto filterOptions)
        {
            return Ok(ResultOutDtoBuilder
                .Success(await _commentService.Filter(filterOptions)));
        }

        // GET: api/comments/5
        [HttpGet("{id}")]
        [ProducesResponseType(200)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<Comment>> GetComment(string id)
        {
            if (!Guid.TryParse(id, out Guid guid))
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
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<object>>> PutComment([FromRoute]string id,[FromBody]Comment comment)
        {
            if (!Guid.TryParse(id, out Guid guid)||guid!=comment.Id)
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
        [ProducesResponseType(200)]
        [ProducesResponseType(400)]
        public async Task<ActionResult<ResultOutDto<Comment>>> PostComment([FromBody]CommentCreateInDto createOptions)
        {
           try
           {
           var comment = await _commentService.Create(createOptions);
           return Ok(ResultOutDtoBuilder.Success(comment));
           }
           catch (NotExistedException e)
           {
               return BadRequest(ResultOutDtoBuilder.Fail<Comment>(e,"No that post."));
           }


        }

        // DELETE: api/Comments/5
        [HttpDelete("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<object>>> DeleteComment([FromRoute]string id)
        {
            if (!Guid.TryParse(id, out Guid guid))
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