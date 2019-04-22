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
    public class PostService
    {
        private readonly EruContext _context;
        public PostService(
            EruContext context)
        {
            _context = context;
        }

        public async Task<List<Post>> Filter(PostFilterInDto filterOptions)
        {
            var filtered = _context.Posts
                .Include(p => p.User)
                .Include(p => p.PostTagAssociations)
                .TimeRangeFilter(filterOptions)
                .Where(post =>
                    ((string.IsNullOrWhiteSpace(filterOptions.TitleMatch) ||
                      post.Title.Contains(filterOptions.TitleMatch))
                     && (filterOptions.CategoryId == null || filterOptions.CategoryId == post.CategoryId)
                     && (filterOptions.StatusId == null || filterOptions.StatusId == post.StatusId)
                     && (filterOptions.TagIds == null ||
                         post.PostTagAssociations.Any(a => filterOptions.TagIds.Contains(a.TagId)))
                     && (filterOptions.UserId == null || filterOptions.UserId == post.UserId)));
            var ordered = filterOptions.CreateTimeDesc
                ? filtered.OrderByDescending(p => p.CreateTime)
                : filtered.OrderBy(p => p.CreateTime);
            
            return await ordered.SkipTakePaging(filterOptions).ToListAsync();
        }

        public async Task<Post> Get(Guid id)
        {
            var post = await _context.Posts
                .Include(p => p.Comments)
                    .ThenInclude(c=>c.User)
                .Include(p => p.User)
                .Include(p => p.PostTagAssociations)
                .FirstOrDefaultAsync(p => p.Id == id);
            if (post == null)
            {
                throw new NotExistedException();
            }

            return post;
        }

        public async Task<Post> Create(PostCreateInDto createOptions)
        {
            using (var transaction = await _context.Database.BeginTransactionAsync())
            {
                var now = DateTime.Now;
                var newPost = new Post
                {
                    StatusId = createOptions.StatusId,
                    CategoryId = createOptions.CategoryId,
                    CreateTime = now,
                    UpdateTime = now,
                    UserId = createOptions.UserId,
                    Title = createOptions.Title,
                    Content = createOptions.Content,
                    Description = createOptions.Description
                };
                await _context.Posts.AddAsync(newPost);
                await _context.SaveChangesAsync();
                await _context.PostTagAssociations.AddRangeAsync(createOptions.TagIds.Select(t=>new PostTagAssociation {TagId = t, PostId = newPost.Id}));
                await _context.SaveChangesAsync();
                transaction.Commit();
                return newPost;
            }
        }

        public async Task<Post> Update(Post post)
        {
            if (!await _context.Posts.AnyAsync(p => p.Id == post.Id))
            {
                throw new NotExistedException();
            }
            _context.Entry(post).State = EntityState.Modified;
            if (post.PostTagAssociations != null)
            {
                _context.Entry(post.PostTagAssociations).State = EntityState.Modified;
            }

            post.UpdateTime = DateTime.Now;
            await _context.SaveChangesAsync();
            return post;
        }

        public async Task<Post> Remove(Guid id)
        {
            var post = await _context.Posts.FindAsync(id);
            if (post==null)
            {
                throw new NotExistedException();
            }

            _context.Posts.Remove(post);
            await _context.SaveChangesAsync();
            return post;
        }
    }
}
