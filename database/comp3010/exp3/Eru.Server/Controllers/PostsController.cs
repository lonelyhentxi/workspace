using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Eru.Server.Data.Models;
using Microsoft.AspNetCore.Mvc;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Eru.Server.Services;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class PostsController : ControllerBase
    {
        private readonly PostService _postService;
        public PostsController(PostService postService)
        {
            _postService = postService;
        }

        // GET: api/post
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto< List<Post>>>> GetPosts([FromQuery] PostFilterInDto filterOptions)
        {
            return Ok(ResultOutDtoBuilder.Success(await _postService.Filter(filterOptions)));
        }

        // GET: api/post/5
        [HttpGet("{id}")]
        [ProducesResponseType(200)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<Post>>> GetPost([FromRoute] string id)
        {
            if (!Guid.TryParse(id, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<Post>(new FormatException(),"Error guid id format."));
            }

            try
            {
                return Ok(ResultOutDtoBuilder.Success(await _postService.Get(guid)));
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder
                    .Fail<Post>(e, "Not exist."));
            }
        }

        // POST: api/post
        [HttpPost]
        [ProducesResponseType(200)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<Post>>> PostPost([FromBody] PostCreateInDto createOptions)
        {
            try
            {
                var post = await _postService.Create(createOptions);
                return Ok(ResultOutDtoBuilder.Success(post));
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder.Fail<Post>(e, "Not exist"));
            }
        }

        // PUT: api/post/5
        [HttpPut("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<object>>> PutPost([FromRoute]string id, [FromBody] Post post)
        {
            if (!Guid.TryParse(id, out Guid guid)||guid!=post.Id)
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<Post>(new FormatException(), "Error id format."));
            }

            try
            {
                await _postService.Update(post);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder
                    .Fail<Post>(e, "Not exist."));
            }
        }

        // DELETE: api/post/5
        [HttpDelete("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<object>>> DeletePost([FromRoute] string id)
        {
            if (!Guid.TryParse(id, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<Post>(new FormatException(), "Error id format."));
            }

            try
            {
                await _postService.Remove(guid);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder
                    .Fail<Post>(e, "Not exist."));
            }
        }
    }
}