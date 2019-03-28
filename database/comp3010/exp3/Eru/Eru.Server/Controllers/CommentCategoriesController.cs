using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using Eru.Server.Models;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class CommentCategoriesController : ControllerBase
    {
        private readonly EruContext _context;

        public CommentCategoriesController(EruContext context)
        {
            _context = context;
        }

        // GET: api/CommentCategories
        [HttpGet]
        public async Task<ActionResult<IEnumerable<CommentCategory>>> GetCommentCategories()
        {
            return await _context.CommentCategories.ToListAsync();
        }

        // GET: api/CommentCategories/5
        [HttpGet("{id}")]
        public async Task<ActionResult<CommentCategory>> GetCommentCategory(int id)
        {
            var commentCategory = await _context.CommentCategories.FindAsync(id);

            if (commentCategory == null)
            {
                return NotFound();
            }

            return commentCategory;
        }

        // PUT: api/CommentCategories/5
        [HttpPut("{id}")]
        public async Task<IActionResult> PutCommentCategory(int id, CommentCategory commentCategory)
        {
            if (id != commentCategory.Id)
            {
                return BadRequest();
            }

            _context.Entry(commentCategory).State = EntityState.Modified;

            try
            {
                await _context.SaveChangesAsync();
            }
            catch (DbUpdateConcurrencyException)
            {
                if (!CommentCategoryExists(id))
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

        // POST: api/CommentCategories
        [HttpPost]
        public async Task<ActionResult<CommentCategory>> PostCommentCategory(CommentCategory commentCategory)
        {
            _context.CommentCategories.Add(commentCategory);
            await _context.SaveChangesAsync();

            return CreatedAtAction("GetCommentCategory", new { id = commentCategory.Id }, commentCategory);
        }

        // DELETE: api/CommentCategories/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<CommentCategory>> DeleteCommentCategory(int id)
        {
            var commentCategory = await _context.CommentCategories.FindAsync(id);
            if (commentCategory == null)
            {
                return NotFound();
            }

            _context.CommentCategories.Remove(commentCategory);
            await _context.SaveChangesAsync();

            return commentCategory;
        }

        private bool CommentCategoryExists(int id)
        {
            return _context.CommentCategories.Any(e => e.Id == id);
        }
    }
}
