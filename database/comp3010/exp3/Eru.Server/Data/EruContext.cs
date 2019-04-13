using System;
using System.Linq;
using Eru.Server.Data.Models;
using Microsoft.AspNetCore.Identity;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Data
{
    public class EruContext : DbContext
    {
        public EruContext(DbContextOptions<EruContext> options) : base(options)
        {
        }

        #region dbset_declare

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

        #endregion

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            #region ENTITIES_BUILD

            var userEntity = modelBuilder.Entity<User>();
            var applicationEntity = modelBuilder.Entity<Application>();
            var commentEntity = modelBuilder.Entity<Comment>();
            var postEntity = modelBuilder.Entity<Post>();
            var userProfileEntity = modelBuilder.Entity<UserProfile>();
            var applicationProfileEntity = modelBuilder.Entity<ApplicationProfile>();
            var roleEntity = modelBuilder.Entity<Role>();
            var permissionEntity = modelBuilder.Entity<Permission>();
            var rolePermissionEntity = modelBuilder.Entity<RolePermissionAssociation>();
            var userRoleEntity = modelBuilder.Entity<UserRoleAssociation>();
            var postCategoryEntity = modelBuilder.Entity<PostCategory>();
            var postStatusEntity = modelBuilder.Entity<PostStatus>();
            var postTagEntity = modelBuilder.Entity<PostTag>();
            var commentCategoryEntity = modelBuilder.Entity<CommentCategory>();
            var commentStatusEntity = modelBuilder.Entity<CommentStatus>();
            var enrollmentEntity = modelBuilder.Entity<Enrollment>();
            var postTagAssociationEntity = modelBuilder.Entity<PostTagAssociation>();

            #endregion

            #region multi_primary_key
                
            enrollmentEntity.HasKey(e => new { e.ApplicationId, e.UserId });
            postTagAssociationEntity.HasKey(a => new { a.PostId, a.TagId });
            userRoleEntity.HasKey(a => new { a.UserId, a.RoleId });
            rolePermissionEntity.HasKey(a => new { a.RoleId, a.PermissionId });

            #endregion

            #region unique_index

            permissionEntity.HasAlternateKey(p => p.Name);
            roleEntity.HasAlternateKey(r => r.Name);
            userEntity.HasAlternateKey(r => r.Name);
            applicationEntity.HasAlternateKey(a => a.Name);
            postTagEntity.HasAlternateKey(p => p.Name);
            postCategoryEntity.HasAlternateKey(p => p.Name);
            postStatusEntity.HasAlternateKey(p => p.Name);
            commentCategoryEntity.HasAlternateKey(c => c.Name);
            commentStatusEntity.HasAlternateKey(c => c.Name);

            #endregion

            #region common_index

            userEntity.HasIndex(u => u.CreateTime);
            userEntity.HasIndex(u => u.UpdateTime);
            applicationEntity.HasIndex(a => a.CreateTime);
            applicationEntity.HasIndex(a => a.UpdateTime);
            postEntity.HasIndex(p => p.CreateTime);
            postEntity.HasIndex(p => p.UpdateTime);
            commentEntity.HasIndex(c => c.CreateTime);
            commentEntity.HasIndex(c => c.UpdateTime);

            #endregion

            #region enitities_relationship

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
            postEntity
                .HasOne(p => p.Status)
                .WithMany(s => s.Posts)
                .IsRequired(true)
                .OnDelete(DeleteBehavior.Restrict);
            postEntity
                .HasOne(p => p.Category)
                .WithMany(c => c.Posts)
                .IsRequired(false)
                .OnDelete(DeleteBehavior.SetNull);
            postEntity
                .HasOne(p => p.User)
                .WithMany(u => u.Posts)
                .IsRequired(true)
                .OnDelete(DeleteBehavior.Restrict);
            commentEntity
                .HasOne(c => c.Parent)
                .WithMany(c => c.Children)
                .OnDelete(DeleteBehavior.Restrict)
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
                .IsRequired(true)
                .OnDelete(DeleteBehavior.Restrict);
            commentEntity
                .HasOne(c => c.Post)
                .WithMany(p => p.Comments)
                .IsRequired(true)
                .OnDelete(DeleteBehavior.Restrict);
            
            #endregion
        }
    }
}