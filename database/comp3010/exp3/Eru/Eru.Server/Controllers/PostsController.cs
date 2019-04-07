using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Data.Utils;
using Microsoft.AspNetCore.Mvc;
using Eru.Server.Dtos;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Diagnostics;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class PostsController : ControllerBase
    {
        private readonly EruContext _context;

        public PostsController(EruContext context)
        {
            _context = context;
        }

        // GET: api/post
        [HttpGet]
        public async Task<ActionResult<List<Post>>> GetPosts([FromQuery] PostFilterInDto filterOptions)
        {
            var mid = _context.Posts
                .Include(p => p.User)
                .Include(p => p.PostTagAssociations)
                .TimeRangeFilter(filterOptions)
                .Where(post =>
                    ((string.IsNullOrWhiteSpace(filterOptions.TitleMatch) ||
                      post.Title.Contains(filterOptions.TitleMatch))
                     && (filterOptions.CategoryId == null || filterOptions.CategoryId == post.CategoryId)
                     && (filterOptions.StatusId == null || filterOptions.StatusId == post.StatusId)
                     && (filterOptions.TagIds == null ||
                         post.PostTagAssociations.Any(a => filterOptions.TagIds.Contains(a.TagId)))
                     && (filterOptions.UserId == null || filterOptions.UserId == post.UserId)));
            if (filterOptions.CreateTimeDesc)
            {
                return await mid.OrderByDescending(c => c.CreateTime).SkipTakePaging(filterOptions).ToListAsync();
            }
            else
            {
                return await mid.OrderBy(c => c.CreateTime).SkipTakePaging(filterOptions).ToListAsync();
            }
        }

        // GET: api/post/5
        [HttpGet("{id}", Name = "Get")]
        public string GetPost(int id)
        {
            return "value";
        }

        // POST: api/post
        [HttpPost]
        public void PostPost([FromBody] string value)
        {
        }

        // PUT: api/post/5
        [HttpPut("{id}")]
        public void PutPost(int id, [FromBody] string value)
        {
        }

        // DELETE: api/post/5
        [HttpDelete("{id}")]
        public void DeletePost(int id)
        {
        }
    }
}