using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;
using Eru.Server.Data.Models;
using Microsoft.AspNetCore.Identity;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;

namespace Eru.Server.Data
{
    public static class SeedData
    {
        public static void Initialize(IServiceProvider serviceProvider)
        {
            using (var context = new EruContext(serviceProvider.GetRequiredService<DbContextOptions<EruContext>>()))
            {
                if (context.Users.Any(u=>u.Name=="master"))
                {
                    return;
                }

                using (var transaction = context.Database.BeginTransaction())
                {
                    Action<string> setIdentityInsertOn = (string tableName) =>
                    {
                        try
                        {
                            context.Database.ExecuteSqlCommand("SET IDENTITY_INSERT dbo." + tableName + " ON;");
                        }
                        catch (Exception e)
                        {
                            //Console.WriteLine(e);
                        }
                    };
                    Action<string> setIdentityInsertOff = (string tableName) =>
                    {
                        try
                        {
                            context.Database.ExecuteSqlCommand("SET IDENTITY_INSERT dbo." + tableName + " OFF;");
                        }
                        catch (Exception e)
                        {
                            //Console.WriteLine(e);
                        }
                    };

                    var passwordHasher = new PasswordHasher<string>();
                    var now = DateTime.Now;
                    var masterGuid = Guid.Parse("2a2eec6f-58f5-44e1-95e8-887955c17533");
                    context.Users.AddRange(new User()
                    {
                        Id = masterGuid,
                        Name = "master",
                        Password = passwordHasher.HashPassword(masterGuid.ToString(), "master"),
                        Description = "blog master",
                        Avatar = "https://avatars3.githubusercontent.com/u/23011677?s=460&v=4",
                        Url = "https://github.com/lonelyhentai",
                        CreateTime = now,
                        UpdateTime = now
                    });

                    context.UserProfiles.AddRange(new UserProfile()
                    {
                        Id = masterGuid,
                        Misc = "{}",
                        Setting = "{}",
                    });
                    context.Roles.AddRange(
                        new Role
                        {
                            Id =1,
                            Description = "Administration",
                            Name = "admin"
                        },
                        new Role
                        {
                            Id=2,
                            Description = "Author",
                            Name = "author",
                        },
                        new Role
                        {
                            Id =3,
                            Description = "User",
                            Name = "user"
                        });
                    setIdentityInsertOn("Roles");
                    context.SaveChanges();
                    setIdentityInsertOff("Roles");

                    // {WR}/{LEVEL}/{SCOPE}/{RESOURCE}
                    var permissions = new Permission[]
                    {
                        new Permission
                        {
                            Id =1,
                            Description = "write all resource",
                            Name = "w/a/a/all", // write/abstract/all/all
                        },
                        new Permission
                        {
                            Id =2,
                            Description = "read all resource detailed",
                            Name = "w/d/a/all" // write/detail/all/all
                        },
                        new Permission
                        {
                            Id = 3,
                            Description = "read all resource",
                            Name = "r/a/a/all" // read/abstract/all/all
                        },
                        new Permission
                        {
                            Id = 4,
                            Description = "read self resource detailed",
                            Name = "r/d/s/all"
                        },
                        new Permission
                        {
                            Id = 5,
                            Description = "write self user detailed",
                            Name = "w/d/s/user"
                        },
                        new Permission
                        {
                            Id =6,
                            Description = "write self blog",
                            Name = "w/_/s/blog",
                        },
                        new Permission
                        {
                            Id =7,
                            Description = "write self comment",
                            Name = "w/_/s/comment"
                        }
                    };
                    context.Permissions.AddRange(
                        permissions
                    );
                    var rolePermissions = permissions
                        .Select(p => new RolePermissionAssociation {RoleId = 1, PermissionId = p.Id}).ToList();
                    rolePermissions.AddRange(permissions
                        .Where(p => p.Id == 3 || p.Id == 4 || p.Id == 5 || p.Id == 6 || p.Id == 7)
                        .Select(p => new RolePermissionAssociation {RoleId = 2, PermissionId = p.Id}).ToList());
                    rolePermissions.AddRange(permissions
                        .Where(p => p.Id == 3 || p.Id == 4 || p.Id == 6 || p.Id == 7)
                        .Select(p => new RolePermissionAssociation {RoleId = 3, PermissionId = p.Id}).ToList());
                    context.RolePermissionAssociations.AddRange(rolePermissions);
                    context.UserRoleAssociations.AddRange(new UserRoleAssociation[]
                    {
                        new UserRoleAssociation() {UserId = masterGuid, RoleId = 1},
                        new UserRoleAssociation() {UserId = masterGuid, RoleId = 2},
                        new UserRoleAssociation() {UserId = masterGuid, RoleId = 3},
                    });

                    
                    
                    setIdentityInsertOn("Permissions");
                    context.SaveChanges();
                    setIdentityInsertOff("Permissions");
                    transaction.Commit();
                }
            }
        }
    }
}