using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Dtos;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using Eru.Server.Utils;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class CommentsController : ControllerBase
    {
        private readonly EruContext _context;

        public CommentsController(EruContext context)
        {
            _context = context;
        }

        // GET: api/Comments
        [HttpGet]
        public async Task<ActionResult<IEnumerable<Comment>>> GetComments([FromQuery] CommentFilterInDto filterOptions)
        {
            var mid = _context.Comments
                .Include(c => c.Post)
                .Include(c => c.User)
                .Where(c => (
                    (filterOptions.ParentId == null || filterOptions.ParentId == c.ParentId)
                    && (filterOptions.PostId == null || filterOptions.PostId == c.PostId)
                    && (filterOptions.UserId == null || filterOptions.UserId == c.UserId)
                    && (filterOptions.StatusId == null || filterOptions.StatusId == c.StatusId)
                    && (filterOptions.CategoryId == null || filterOptions.CategoryId == c.CategoryId)
                ));
            if (filterOptions.CreateTimeDesc)
            {
                return await mid.OrderByDescending(c => c.CreateTime).SkipTakePaging(filterOptions).ToListAsync();
            }
            else
            {
                return await mid.OrderBy(c => c.CreateTime).SkipTakePaging(filterOptions).ToListAsync();
            }
        }

        // GET: api/Comments/5
        [HttpGet("{id}")]
        public async Task<ActionResult<Comment>> GetComment(string id)
        {
            var comment = await _context.Comments.FindAsync(id);

            if (comment == null)
            {
                return NotFound();
            }

            return comment;
        }

        // PUT: api/Comments/5
        [HttpPut("{id}")]
        public async Task<IActionResult> PutComment(string id, Comment comment)
        {
            if (id != comment.Id)
            {
                return BadRequest();
            }

            _context.Entry(comment).State = EntityState.Modified;

            try
            {
                await _context.SaveChangesAsync();
            }
            catch (DbUpdateConcurrencyException)
            {
                if (!CommentExists(id))
                {
                    return NotFound();
                }
                else
                {
                    throw;
                }
            }

            return NoContent();
        }

        // POST: api/Comments
        [HttpPost]
        public async Task<ActionResult<Comment>> PostComment(Comment comment)
        {
            _context.Comments.Add(comment);
            await _context.SaveChangesAsync();

            return CreatedAtAction("GetComment", new {id = comment.Id}, comment);
        }

        // DELETE: api/Comments/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<Comment>> DeleteComment(string id)
        {
            var comment = await _context.Comments.FindAsync(id);
            if (comment == null)
            {
                return NotFound();
            }

            _context.Comments.Remove(comment);
            await _context.SaveChangesAsync();

            return comment;
        }

        private bool CommentExists(string id)
        {
            return _context.Comments.Any(e => e.Id == id);
        }
    }
}