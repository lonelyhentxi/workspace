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
    public class PostCategoryService
    {
        private readonly EruContext _context;
        public PostCategoryService(EruContext context)
        {
            _context = context;
        }

        public async Task<List<PostCategory>> GetAll()
        {
            return await _context.PostCategories.ToListAsync();
        }

        public async Task<PostCategory> Create(TagCreateInDto createOptions)
        {
            if (await _context.PostCategories.AnyAsync(t => t.Name == createOptions.Name))
            {
                throw new ExistedConflictException();
            }

            var newCategory = new PostCategory { Name = createOptions.Name, Description = createOptions.Description };
            await _context.PostCategories.AddAsync(newCategory);
            await _context.SaveChangesAsync();
            return newCategory;
        }

        public async Task<PostCategory> Remove(int id)
        {
            var category = await _context.PostCategories.FindAsync(id);
            if (category == null)
            {
                throw new NotExistedException();
            }

            _context.PostCategories.Remove(category);
            await _context.SaveChangesAsync();
            return category;
        }

        public async Task<PostCategory> Update(PostCategory category)
        {
            if (!await _context.PostCategories.AnyAsync(t => t.Id == category.Id))
            {
                throw new NotExistedException();
            }

            if (!await _context.PostCategories.AnyAsync(t => t.Name == category.Name && t.Id != category.Id))
            {
                throw new ExistedConflictException();
            }

            _context.Entry(category).State = EntityState.Modified;
            await _context.SaveChangesAsync();
            return category;
        }

        public async Task<List<GroupCountOutDto<int?>>> Group()
        {
            return await _context.Posts.GroupBy(p => p.CategoryId)
                .Select(g => new GroupCountOutDto<int?> {Count = g.Count(), Id = g.Key}).ToListAsync();
        }
    }
}
