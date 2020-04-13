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
    public class PostCategoriesController : ControllerBase
    {
        private readonly PostCategoryService _categoryService;

        public PostCategoriesController(PostCategoryService categoryService)
        {
            _categoryService = categoryService;
        }

        // GET: api/postCategories
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<PostCategory>>>> GetPostCategories()
        {
            return Ok(ResultOutDtoBuilder.Success(await _categoryService.GetAll()));
        }

        [HttpPost]
        [ProducesResponseType(200)]
        [ProducesResponseType(409)]
        public async Task<ActionResult<ResultOutDto<PostCategory>>> PostPostCategory(
            [FromBody] TagCreateInDto createOptions)
        {
            try
            {
                return Ok(ResultOutDtoBuilder.Success(await _categoryService.Create(createOptions)));
            }
            catch (ExistedConflictException e)
            {
                return Conflict(ResultOutDtoBuilder.Fail<PostCategory>(e, "Category name existed."));
            }
        }

        [HttpPut("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        [ProducesResponseType(409)]
        public async Task<ActionResult<ResultOutDto<object>>> PutPostCategories(
            [FromRoute] int id, [FromBody] PostCategory category
        )
        {
            if (id != category.Id)
            {
                return BadRequest(ResultOutDtoBuilder.Fail<object>(new FormatException(), "error id format"));
            }

            try
            {
                await _categoryService.Update(category);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder.Fail<object>(e, "Not exist."));
            }
            catch (ExistedConflictException e)
            {
                return Conflict(ResultOutDtoBuilder.Fail<object>(e, "Conflict name."));
            }
        }

        [HttpDelete("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<object>>> DeletePostCategory(
            [FromRoute] int id)
        {
            try
            {
                await _categoryService.Remove(id);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder.Fail<object>(e, "Not exist."));
            }
        }

        [HttpGet("groupCount")]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<List<GroupCountOutDto<int?>>>>> GroupCount()
        {
            return Ok(ResultOutDtoBuilder.Success(await _categoryService.Group()));
        }
    }
}
