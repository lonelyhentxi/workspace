using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Eru.Server.Services;
using Microsoft.AspNetCore.Mvc;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class CommentCategoriesController : ControllerBase
    {
        private readonly CommentCategoryService _categoryService;

        public CommentCategoriesController(CommentCategoryService categoryService)
        {
            _categoryService = categoryService;
        }

        // GET: api/commentCategories
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<CommentCategory>>>> GetCommentCategories()
        {
            return Ok(ResultOutDtoBuilder.Success(await _categoryService.GetAll()));
        }
    }
}
