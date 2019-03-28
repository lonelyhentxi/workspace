using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Models
{
    public class EruContext : DbContext
    {
        public EruContext(DbContextOptions<EruContext> options) : base(options)
        {
        }

        public DbSet<User> Users { get; set; }
        public DbSet<UserProfile> UserProfiles { get; set; }
        public DbSet<Role> Roles { get; set; }
        public DbSet<Permission> Permissions { get; set; }
        public DbSet<UserRoleAssociation> UserRoleAssociations { get; set; }
        public DbSet<RolePermissionAssociation> RolePermissionAssociations { get; set; }
        public DbSet<Post> Posts { get; set; }
        public DbSet<Comment> Comments { get; set; }
        public DbSet<PostStatus> PostStatuses { get; set; }
        public DbSet<PostCategory> PostCategories { get; set; }
        public DbSet<CommentStatus> CommentStatuses { get; set; }
        public DbSet<CommentCategory> CommentCategories { get; set; }
        public DbSet<PostTag> PostTags { get; set; }
        public DbSet<PostTagAssociation> PostTagAssociations { get; set; }
        public DbSet<Application> Applications { get; set; }
        public DbSet<ApplicationProfile> ApplicationProfiles { get; set; }
        public DbSet<Enrollment> Enrollments { get; set; }

        protected void OnCommentEntityCreating(ModelBuilder modelBuilder)
        {
            var commentEntity = modelBuilder.Entity<Comment>();
            commentEntity
                .HasOne(c => c.Parent)
                .WithMany(c => c.Children)
                .OnDelete(DeleteBehavior.SetNull)
                .IsRequired(false);
            commentEntity
                .HasOne(c => c.Status)
                .WithMany(s => s.Comments)
                .IsRequired(true)
                .OnDelete(DeleteBehavior.Restrict);
            commentEntity
                .HasOne(c => c.Category)
                .WithMany(t => t.Comments)
                .IsRequired(false)
                .OnDelete(DeleteBehavior.SetNull);
            commentEntity
                .HasOne(c => c.User)
                .WithMany(u => u.Comments)
                .IsRequired(false)
                .OnDelete(DeleteBehavior.SetNull);
            commentEntity
                .HasOne(c => c.Post)
                .WithMany(p => p.Comments)
                .IsRequired(true)
                .OnDelete(DeleteBehavior.Cascade);
        }

        protected void OnPostEntityCreating(ModelBuilder modelBuilder)
        {
            var postEntity = modelBuilder.Entity<Post>();
            postEntity
                .HasOne(p => p.Status)
                .WithMany(s => s.Posts)
                .IsRequired(true)
                .OnDelete(DeleteBehavior.Restrict);
            postEntity
                .HasOne(p => p.Category)
                .WithMany(c => c.Posts)
                .IsRequired(true)
                .OnDelete(DeleteBehavior.SetNull);
            postEntity
                .HasOne(p => p.User)
                .WithMany(u => u.Posts)
                .IsRequired(false)
                .OnDelete(DeleteBehavior.SetNull);
        }

        protected void OnAssociationKeyCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<Enrollment>()
                .HasKey(e => new { e.ApplicationId, e.UserId });

            modelBuilder.Entity<PostTagAssociation>()
                .HasKey(a => new { a.PostId, a.TagId });

            modelBuilder.Entity<UserRoleAssociation>()
                .HasKey(a => new { a.UserId, a.RoleId });

            modelBuilder.Entity<RolePermissionAssociation>()
                .HasKey(a => new { a.RoleId, a.PermissionId });
        }

        protected void OnSingleProfileCreating(ModelBuilder modelBuilder)
        {
            var userEntity = modelBuilder.Entity<User>();
            var applicationEntity = modelBuilder.Entity<Application>();

            userEntity
                .HasOne(u => u.Profile)
                .WithOne(up => up.User)
                .OnDelete(DeleteBehavior.Cascade)
                .IsRequired(true);
            applicationEntity
                .HasOne(a => a.Profile)
                .WithOne(ap => ap.Application)
                .OnDelete(DeleteBehavior.Cascade)
                .IsRequired(true);
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            OnAssociationKeyCreating(modelBuilder);
            OnSingleProfileCreating(modelBuilder);
            OnCommentEntityCreating(modelBuilder);
            OnPostEntityCreating(modelBuilder);
        }
    }
}