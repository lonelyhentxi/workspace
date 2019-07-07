using System.Collections.Generic;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class PostStatusService
    {
        private readonly EruContext _context;
        public PostStatusService(EruContext context)
        {
            _context = context;
        }

        public async Task<List<PostStatus>> GetAll()
        {
            return await _context.PostStatuses.ToListAsync();
        }
    }
}
