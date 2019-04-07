using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.EntityFrameworkCore;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Hosting;
using Newtonsoft.Json.Serialization;
using Newtonsoft.Json;
using log4net.Repository;
using log4net;
using log4net.Config;
using System.IO;
using Eru.Server.Data;

#region CUSTOM_IMPORT

#endregion

namespace Eru.Server
{
    public class Startup
    {
        public IConfiguration Configuration { get; set; }
        public static ILoggerRepository repository { get; set; }

        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
            repository = LogManager.CreateRepository("eru");
            XmlConfigurator.Configure(repository, new FileInfo("log4net.config.xml"));
        }

        public void ConfigureServices(IServiceCollection services)
        {
            #region DB_CONTEXT_POOL_SERVICE
            services.AddDbContextPool<EruContext>(opt => { opt.UseInMemoryDatabase("eru"); });

            #endregion

            #region MVC_SERVICE

            services.AddMvc()
                .SetCompatibilityVersion(CompatibilityVersion.Version_3_0)
                .AddNewtonsoftJson(options =>
                {
                    options.SerializerSettings.ContractResolver =
                        new DefaultContractResolver();
                    options.SerializerSettings.Formatting = Formatting.Indented;
                });

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
                app.UseHsts();
            }

            app.UseMvc();
        } 
    }
}