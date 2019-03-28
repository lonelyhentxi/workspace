using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Models;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.HttpsPolicy;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Swashbuckle.AspNetCore.Swagger;
using Microsoft.EntityFrameworkCore;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;
using AutoMapper;
using Microsoft.AspNetCore.Http;

namespace Eru.Server
{
    public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.Configure<CookiePolicyOptions>(options =>
            {
                options.CheckConsentNeeded = context => true;
                options.MinimumSameSitePolicy = SameSiteMode.None;
            });
            services.AddDbContextPool<EruContext>(opt => { opt.UseInMemoryDatabase("eru"); });
            services.AddMvc()
                .SetCompatibilityVersion(CompatibilityVersion.Version_3_0)
                .AddNewtonsoftJson(options =>
                    {
                        options.SerializerSettings.ContractResolver = new DefaultContractResolver();
                        options.SerializerSettings.Formatting = Formatting.Indented;
                    }
                );

            services.AddSwaggerGen(c =>
            {
                c.SwaggerDoc("v1", new Info
                {
                    Title = "Eru API", Version = "v1",
                    Description = "Eru Blog System API Documentation",
                    TermsOfService = "None",
                    Contact = new Contact
                    {
                        Name = "Zhou Yeheng",
                        Email = "master@evernightfireworks.com"
                    },
                    License = new License
                    {
                        Name = "Common Creative 3.0",
                        Url = "https://creativecommons.org/licenses/by/3.0/"
                    }
                });
            });
        }

        public void AutoMapperConfigure(IApplicationBuilder app)
        {
            Mapper.Initialize(cfg =>
            {
            });
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            /*
            else
            {
                // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
                app.UseHsts();
            }
            */

            // app.UseHttpsRedirection();
            // Enable middleware to serve generated Swagger as a Json endpoint
            app.UseSwagger();
            app.UseSwaggerUI(c => { c.SwaggerEndpoint("/doc/v1/swagger.json", "Eru API v1"); });
            app.UseMvc();
        }


    }
}