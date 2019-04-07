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
    public class PostStatusController : ControllerBase
    {
        private readonly EruContext _context;

        public PostStatusController(EruContext context)
        {
            _context = context;
        }

        // GET: api/PostStatus
        [HttpGet]
        public async Task<ActionResult<IEnumerable<PostStatus>>> GetPostStatuses()
        {
            return await _context.PostStatuses.ToListAsync();
        }

        // GET: api/PostStatus/5
        [HttpGet("{id}")]
        public async Task<ActionResult<PostStatus>> GetPostStatus(int id)
        {
            var postStatus = await _context.PostStatuses.FindAsync(id);

            if (postStatus == null)
            {
                return NotFound();
            }

            return postStatus;
        }

        // PUT: api/PostStatus/5
        [HttpPut("{id}")]
        public async Task<IActionResult> PutPostStatus(int id, PostStatus postStatus)
        {
            if (id != postStatus.Id)
            {
                return BadRequest();
            }

            _context.Entry(postStatus).State = EntityState.Modified;

            try
            {
                await _context.SaveChangesAsync();
            }
            catch (DbUpdateConcurrencyException)
            {
                if (!PostStatusExists(id))
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

        // POST: api/PostStatus
        [HttpPost]
        public async Task<ActionResult<PostStatus>> PostPostStatus(PostStatus postStatus)
        {
            _context.PostStatuses.Add(postStatus);
            await _context.SaveChangesAsync();

            return CreatedAtAction("GetPostStatus", new { id = postStatus.Id }, postStatus);
        }

        // DELETE: api/PostStatus/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<PostStatus>> DeletePostStatus(int id)
        {
            var postStatus = await _context.PostStatuses.FindAsync(id);
            if (postStatus == null)
            {
                return NotFound();
            }

            _context.PostStatuses.Remove(postStatus);
            await _context.SaveChangesAsync();

            return postStatus;
        }

        private bool PostStatusExists(int id)
        {
            return _context.PostStatuses.Any(e => e.Id == id);
        }
    }
}
