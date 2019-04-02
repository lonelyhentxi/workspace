using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
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
    public class ApplicationsController : ControllerBase
    {
        private readonly EruContext _context;

        public ApplicationsController(EruContext context)
        {
            _context = context;
        }

        // GET: api/applications
        [HttpGet]
        public async Task<ActionResult<IEnumerable<Application>>> GetApplications(
            [FromQuery] ApplicationFilterInDto filterOptions)
        {
            var mid = _context.Applications
                .TimeRangeFilter(filterOptions)
                .Where(app =>
                    (string.IsNullOrWhiteSpace(filterOptions.NameMatch) || app.Name.Contains(filterOptions.NameMatch)));
            if (filterOptions.CreateTimeDesc)
            {
                return await mid.OrderByDescending(c => c.CreateTime).SkipTakePaging(filterOptions).ToListAsync();
            }
            else
            {
                return await mid.OrderBy(c => c.CreateTime).SkipTakePaging(filterOptions).ToListAsync();
            }
        }

        // GET: api/applications/5
        [HttpGet("{id}")]
        public async Task<ActionResult<Application>> GetApplication([StringLength(32)] string id)
        {
            var application = await _context.Applications
                .Include(a => a.Profile)
                .Include(a => a.Enrollments)
                .FirstAsync(a => a.Id == id);

            if (application == null)
            {
                return NotFound();
            }

            return application;
        }

        // PUT: api/applications/5
        
        [HttpPut("{id}")]
        public async Task<IActionResult> PutApplication(string id, Application application)
        {
            if (id != application.Id)
            {
                return BadRequest();
            }

            _context.Entry(application).State = EntityState.Modified;

            try
            {
                await _context.SaveChangesAsync();
            }
            catch (DbUpdateConcurrencyException)
            {
                if (!ApplicationExists(id))
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

        // POST: api/Applications
        [HttpPost]
        public async Task<ActionResult<Application>> PostApplication(Application application)
        {
            _context.Applications.Add(application);
            await _context.SaveChangesAsync();

            return CreatedAtAction("GetApplication", new {id = application.Id}, application);
        }

        // DELETE: api/Applications/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<Application>> DeleteApplication(string id)
        {
            var application = await _context.Applications.FindAsync(id);
            if (application == null)
            {
                return NotFound();
            }

            _context.Applications.Remove(application);
            await _context.SaveChangesAsync();

            return application;
        }

        private bool ApplicationExists(string id)
        {
            return _context.Applications.Any(e => e.Id == id);
        }
    }
}