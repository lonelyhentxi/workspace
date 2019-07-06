using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class PostTagService
    {
        private readonly EruContext _context;
        public PostTagService(EruContext context)
        {
            _context = context;
        }

        public async Task<List<PostTag>> GetAll()
        {
            return await _context.PostTags.ToListAsync();
        }

        public async Task<PostTag> Create(TagCreateInDto createOptions)
        {
            if (await _context.PostTags.AnyAsync(t => t.Name == createOptions.Name))
            {
                throw new ExistedConflictException();
            }

            var newTag = new PostTag {Name = createOptions.Name, Description = createOptions.Description};
            await _context.PostTags.AddAsync(newTag);
            await _context.SaveChangesAsync();
            return newTag;
        }

        public async Task<PostTag> Update(PostTag tag)
        {
            if (!await _context.PostTags.AnyAsync(t=>t.Id==tag.Id))
            {
                throw new NotExistedException();
            }

            if (!await _context.PostTags.AnyAsync(t => t.Name == tag.Name && t.Id != tag.Id))
            {
                throw new ExistedConflictException();
            }

            _context.Entry(tag).State = EntityState.Modified;
            await _context.SaveChangesAsync();
            return tag;
        }

        public async Task<PostTag> Remove(int id)
        {
            var tag = await _context.PostTags.FindAsync(id);
            if (tag == null)
            {
                throw new NotExistedException();
            }

            _context.PostTags.Remove(tag);
            await _context.SaveChangesAsync();
            return tag;
        }

        public async Task<List<GroupCountOutDto<int?>>> Group()
        {
            return await _context.PostTags
                .Include(pt=>pt.PostTagAssociations)
                .GroupBy(p => p.Id)
                .Select(g => new GroupCountOutDto<int?> { Count = g.Count(), Id = g.Key }).ToListAsync();
        }
    }
}
