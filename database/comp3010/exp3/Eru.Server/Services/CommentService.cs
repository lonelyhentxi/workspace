using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Data.Utils;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class CommentService
    {
        private readonly EruContext _context;

        public CommentService(EruContext context)
        {
            _context = context;
        }

        public async Task<List<Comment>> Filter(CommentFilterInDto filterOptions)
        {
            var filtered = _context.Comments
                .Include(c => c.Post)
                .Include(c => c.User)
                .Where(c => (
                    (filterOptions.ParentId == null || filterOptions.ParentId == c.ParentId)
                    && (filterOptions.PostId == null || filterOptions.PostId == c.PostId)
                    && (filterOptions.UserId == null || filterOptions.UserId == c.UserId)
                    && (filterOptions.StatusId == null || filterOptions.StatusId == c.StatusId)
                    && (filterOptions.CategoryId == null || filterOptions.CategoryId == c.CategoryId)
                ));
            var ordered = filterOptions.CreateTimeDesc
                ? filtered.OrderByDescending(c => c.CreateTime)
                : filtered.OrderBy(c => c.CreateTime);
            return await ordered.SkipTakePaging(filterOptions).ToListAsync();
        }

        public async Task<Comment> Create(CommentCreateInDto createOptions)
        {
            if (!await _context.Posts.AnyAsync(p => p.Id == createOptions.PostId))
            {
                throw new NotExistedException();
            }
            var now = DateTime.Now;
            var comment = new Comment()
            {
                ParentId = createOptions.ParentId,
                CategoryId = createOptions.CategoryId,
                PostId = createOptions.PostId,
                UserId = createOptions.UserId,
                StatusId = createOptions.StatusId,
                Content = createOptions.Content,
                CreateTime = now,
                UpdateTime = now,
            };
            await _context.Comments.AddAsync(comment);
            await _context.SaveChangesAsync();
            return comment;
        }

        public async Task<Comment> Update(Comment comment)
        {
            if (await _context.Comments.AnyAsync(c => c.Id == comment.Id))
            {
                throw new NotExistedException();
            }

            _context.Entry(comment).State = EntityState.Modified;
            _context.Entry(comment).Property(c => c.UserId).IsModified = false;
            _context.Entry(comment).Property(c => c.PostId).IsModified = false;
            comment.UpdateTime = DateTime.Now;
            await _context.SaveChangesAsync();
            return comment;
        }

        public async Task<Comment> Remove(Guid id)
        {
            var comment = await _context.Comments.FindAsync(id);
            if (comment == null)
            {
                throw new NotExistedException();
            }

            _context.Remove(comment);
            await _context.SaveChangesAsync();
            return comment;
        }

        public async Task<Comment> Get(Guid id)
        {
            var comment = await _context.Comments.FindAsync(id);
            if (comment == null)
            {
                throw new NotExistedException();
            }

            return comment;
        }
    }
}