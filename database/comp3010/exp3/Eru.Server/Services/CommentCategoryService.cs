using System.Collections.Generic;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class CommentCategoryService
    {
        private readonly EruContext _context;
        public CommentCategoryService(EruContext context)
        {
            _context = context;
        }

        public async Task<List<CommentCategory>> GetAll()
        {
            return await _context.CommentCategories.ToListAsync();
        }
    }
}
