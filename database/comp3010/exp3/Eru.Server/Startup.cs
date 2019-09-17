using System;
using System.Reflection;
using Autofac;
using Autofac.Extensions.DependencyInjection;
using Autofac.Extras.DynamicProxy;
using Eru.Server.Configurations;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.EntityFrameworkCore;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Hosting;
using Newtonsoft.Json.Serialization;
using Newtonsoft.Json;
using Eru.Server.Data;
using Eru.Server.Services;
using Eru.Server.Services.Interfaces;
using Microsoft.AspNetCore.Server.Kestrel.Core;

namespace Eru.Server
{
    public class Startup
    {
        public IConfiguration Configuration { get; set; }
        public IContainer ApplicationContainer { get; private set; }

        public Startup(IHostEnvironment env)
        {
            var builder = new ConfigurationBuilder()
                .SetBasePath(env.ContentRootPath)
                .AddJsonFile("appsettings.json", optional: true, reloadOnChange: true)
                .AddJsonFile($"appsettings.{env.EnvironmentName}.json", optional: true)
                .AddEnvironmentVariables();
            Configuration = builder.Build();
        }

        public IServiceProvider ConfigureServices(IServiceCollection services)
        {
            #region config_inject

            var appConfig = Configuration.GetSection("App");
            var authConfig = Configuration.GetSection("Auth");
            var userConfig = Configuration.GetSection("User");
            services.Configure<AppConfig>(appConfig);
            services.Configure<UserConfig>(userConfig);
            services.Configure<AuthenticationConfig>(authConfig);
            services.Configure<KestrelServerOptions>(options =>
            {
                options.AllowSynchronousIO = true;
            });
            services.Configure<IISServerOptions>(options =>
            {
                options.AllowSynchronousIO = true;
            });

            #endregion

            #region db_repository
            services.AddDbContext<EruContext>(opt => { opt.UseSqlServer(Configuration.GetConnectionString("SqlServerLocalDB")); });

            /*services.AddEntityFrameworkNpgsql()
            .AddDbContext<EruContext>(opt =>
            {
                opt.UseNpgsql(Configuration.GetConnectionString("PostgreSQL"));
            });
            */
            #endregion

            #region mvc_service

            services.AddMvc()
                .SetCompatibilityVersion(CompatibilityVersion.Version_3_0)
                .AddNewtonsoftJson(options =>
                {
                    options.SerializerSettings.ContractResolver =
                        new DefaultContractResolver();
                    options.SerializerSettings.Formatting = Formatting.Indented;
                    options.SerializerSettings.ReferenceLoopHandling = ReferenceLoopHandling.Ignore;
                });

            #endregion


            #region swagger_service


            services.AddSwaggerDocument(config =>
            {
                config.PostProcess = document =>
                {
                    document.Info.Version = "v1";
                    document.Info.Title = "ERU API";
                    document.Info.Description = "Eru blog framework api";
                    document.Info.TermsOfService = "None";
                    document.Info.Contact = new NSwag.SwaggerContact
                    {
                        Name = "Zhou Yeheng",
                        Email = "master@evernightfireworks.com",
                        Url = "https://github.com/lonelyhentai"
                    };
                    document.Info.License = new NSwag.SwaggerLicense
                    {
                        Name = "CC3.0",
                        Url = "https://creativecommons.org/licenses/by/3.0/"
                    };
                };
            });

            #endregion

            #region cors_services

            // TODO: when release change it
            services.AddCors(c =>
            {
                c.AddPolicy("AllRequests", policy =>
                {
                    policy
                        .AllowAnyHeader()
                        .AllowAnyMethod()
                        // .AllowCredentials()
                        .AllowAnyOrigin();
                });
            });

            #endregion

            #region autofac_service

            var builder = new ContainerBuilder();
            builder.RegisterAssemblyTypes(Assembly.GetExecutingAssembly());
            builder.RegisterType<UserUrlService>().As<IUserUrlService>();
            builder.RegisterType<UserAvatarService>().As<IUserAvatarService>();
            builder.RegisterType<UserProfileService>().As<IUserProfileService>();
            builder.RegisterType<UrlService>().As<IUrlService>();
            builder.Populate(services);
            ApplicationContainer = builder.Build();
            return new AutofacServiceProvider(ApplicationContainer);

            #endregion
        }

        public void Configure(IApplicationBuilder app, IHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                // app.UseHsts();
            }


            app.UseSwagger();
            app.UseSwaggerUi3();

            // app.UseAuthentication();
            app.UseCors("AllRequests");
            // app.UseHttpsRedirection();
            app.UseStaticFiles();
            app.UseCookiePolicy();
            app.UseStatusCodePages();
            app.UseMvc();
        } 
    }
}