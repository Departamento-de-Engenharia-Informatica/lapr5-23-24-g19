using System;
using System.Net.Http;
using DDDNetCore.Infraestructure.Jobs;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Sequences;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Jobs;
using DDDSample1.Infrastructure.Sequences;
using DDDSample1.Infrastructure.Shared;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;

namespace DDDSample1
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
            // services.AddDbContext<RobDroneDBContext>(opt =>
            //     opt.UseInMemoryDatabase("RobDroneDBContext")
            //     .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>());

            services
                .AddControllers()
                .AddJsonOptions(options =>
                {
                    options.JsonSerializerOptions.PropertyNamingPolicy = null; // Do not apply naming policy
                });
            services.AddDbContext<RobDroneDBContext>(
                options =>
                    options.UseMySql(
                        "Server=localhost;Port=3306;database=RobDroneGO;user=robdronego;password=Password1",
                        new MariaDbServerVersion(new Version(11, 1, 3))
                    )
            ); // Specify your MariaDB version here

            ConfigureMyServices(services);
            services.AddControllers().AddNewtonsoftJson();
            services.AddEndpointsApiExplorer();
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
                app.UseHsts();
            }

            app.UseHttpsRedirection();

            app.UseRouting();

            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });
        }

        public void ConfigureMyServices(IServiceCollection services)
        {
            services.AddTransient<IUnitOfWork, UnitOfWork>();
            // services.AddTransient<IValueObject,ValueObject>();

            // services.AddTransient<ICategoryRepository,CategoryRepository>();
            // services.AddTransient<CategoryService>();

            // services.AddTransient<IProductRepository,ProductRepository>();
            // services.AddTransient<ProductService>();

            // services.AddTransient<IFamilyRepository,FamilyRepository>();
            // services.AddTransient<FamilyService>();

            services.AddTransient<IJobRepository, JobRepository>();
            services.AddTransient<JobService>();
            services.AddTransient<ISequenceRepository, SequenceRepository>();
            // services.AddSingleton<HttpClient>();
            services.AddHttpClient(
                "MyHttpClient",
                client =>
                {
                    client.Timeout = TimeSpan.FromMinutes(5);
                }
            );
            services.AddTransient<IPlanningAdapter, PlanningAdapter>();
        }
    }
}
