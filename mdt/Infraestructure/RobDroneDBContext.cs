using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Categories;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Families;
using DDDSample1.Infrastructure.Categories;
using DDDSample1.Infrastructure.Products;
using DDDSample1.Domain.Jobs;
using DDDSample1.Infrastructure.Jobs;

namespace DDDSample1.Infrastructure
{
    public class RobDroneDBContext : DbContext
    {
        // public DbSet<Category> Categories { get; set; }

        // public DbSet<Product> Products { get; set; }

        // public DbSet<Family> Families { get; set; }
        
        //MDT

        // public DbSet<JobDelivery> Deliveries { get; set; }
        // public DbSet<JobSurveillance> Surveillances { get; set; }
        public DbSet<Job> Jobs { get; set; }

        public RobDroneDBContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            // modelBuilder.ApplyConfiguration(new CategoryEntityTypeConfiguration());
            // modelBuilder.ApplyConfiguration(new ProductEntityTypeConfiguration());
            // modelBuilder.ApplyConfiguration(new FamilyEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new JobEntityTypeConfiguration());

            //TODO: CHANGE TO  JOBTYPE enum,
            modelBuilder.Entity<Job>()
                .HasDiscriminator<string>("JobType") // Or another appropriate discriminator
                .HasValue<JobDelivery>("DELIVERY").HasValue<JobSurveillance>("SURVEILLANCE");

            modelBuilder.Entity<Job>()
                .OwnsOne(job => job.Location, locationBuilder =>
                {
                    locationBuilder.OwnsOne(location => location.StartingPoint);
                    locationBuilder.OwnsOne(location => location.EndingPoint);
                    locationBuilder.Navigation(location => location.StartingPoint).IsRequired();
                    locationBuilder.Navigation(location => location.EndingPoint).IsRequired();
                }).Navigation(job => job.Location).IsRequired();
        }
    }
}