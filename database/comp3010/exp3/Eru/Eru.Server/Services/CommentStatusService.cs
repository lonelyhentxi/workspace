using System.Collections.Generic;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class CommentStatusService
    {
        private readonly EruContext _context;

        public CommentStatusService(EruContext context)
        {
            _context = context;
        }

        public async Task<List<CommentStatus>> GetAll()
        {
            return await _context.CommentStatuses.ToListAsync();
        }
    }
}
