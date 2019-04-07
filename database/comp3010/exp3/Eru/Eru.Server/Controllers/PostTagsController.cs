using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class PostTagsController : ControllerBase
    {
        private readonly EruContext _context;

        public PostTagsController(EruContext context)
        {
            _context = context;
        }

        // GET: api/PostTags
        [HttpGet]
        public async Task<ActionResult<IEnumerable<PostTag>>> GetPostTags()
        {
            return await _context.PostTags.ToListAsync();
        }

        // GET: api/PostTags/5
        [HttpGet("{id}")]
        public async Task<ActionResult<PostTag>> GetPostTag(int id)
        {
            var postTag = await _context.PostTags.FindAsync(id);

            if (postTag == null)
            {
                return NotFound();
            }

            return postTag;
        }

        // PUT: api/PostTags/5
        [HttpPut("{id}")]
        public async Task<IActionResult> PutPostTag(int id, PostTag postTag)
        {
            if (id != postTag.Id)
            {
                return BadRequest();
            }

            _context.Entry(postTag).State = EntityState.Modified;

            try
            {
                await _context.SaveChangesAsync();
            }
            catch (DbUpdateConcurrencyException)
            {
                if (!PostTagExists(id))
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

        // POST: api/PostTags
        [HttpPost]
        public async Task<ActionResult<PostTag>> PostPostTag(PostTag postTag)
        {
            _context.PostTags.Add(postTag);
            await _context.SaveChangesAsync();

            return CreatedAtAction("GetPostTag", new { id = postTag.Id }, postTag);
        }

        // DELETE: api/PostTags/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<PostTag>> DeletePostTag(int id)
        {
            var postTag = await _context.PostTags.FindAsync(id);
            if (postTag == null)
            {
                return NotFound();
            }

            _context.PostTags.Remove(postTag);
            await _context.SaveChangesAsync();

            return postTag;
        }

        private bool PostTagExists(int id)
        {
            return _context.PostTags.Any(e => e.Id == id);
        }
    }
}
