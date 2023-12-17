using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using System;
using DDDSample1.Infrastructure.Jobs;
using DDDSample1.Domain.Jobs;
using System.Linq.Expressions;

namespace DDDSample1.Infrastructure
{
    public class RobDroneDBContext : DbContext
    {
        // public DbSet<Blog> Blogs { get; set; }
        // public DbSet<RssBlog> RssBlogs { get; set; }

        public DbSet<Job> Jobs { get; set; }
        // public DbSet<JobSurveillance> surv { get; set; }
        // public DbSet<JobDelivery> delv { get; set; }

        // public DbSet<Family> fam { get; set; }
        // public DbSet<MomFamily> mom { get; set; }
        // public DbSet<DadFamily> dad { get; set; }

        public RobDroneDBContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new JobEntityTypeConfiguration());
            // modelBuilder.ApplyConfiguration(new BlogEntityTypeConfiguration());
            // modelBuilder.ApplyConfiguration(new FamilyEntityTypeConfiguration());


            //Job is a abstract class that should group 2 types of jobs,DELIVERY, SURVEILLANCE
            modelBuilder.Entity<Job>()
                .HasDiscriminator<JobTypeEnum>("JobType") // Or another appropriate discriminator
                .HasValue<JobDelivery>(JobTypeEnum.DELIVERY)
                .HasValue<JobSurveillance>(JobTypeEnum.SURVEILLANCE);

            // modelBuilder.Entity<JobDelivery>()
            //     .OwnsOne(job => job.PickupContact, contact =>
            //     {
            //         contact.Property(vo => vo.Name).IsRequired();
            //         contact.Property(vo => vo.PhoneNumber).IsRequired();
            //     }
            //     ).Navigation(job => job.PickupContact).IsRequired();
                
            ConfigureContact(modelBuilder.Entity<JobDelivery>(), job => job.PickupContact);
            ConfigureContact(modelBuilder.Entity<JobDelivery>(), job => job.DeliveryContact);
            ConfigureContact(modelBuilder.Entity<JobSurveillance>(), job => job.SurveillanceContact);
            
            // modelBuilder.Entity<JobDelivery>()
            //     .OwnsOne(job => job.DeliveryContact, contact =>
            //     {
            //         contact.Property(vo => vo.Name).IsRequired();
            //         contact.Property(vo => vo.PhoneNumber).IsRequired();
            //     }
            //     ).Navigation(job => job.DeliveryContact).IsRequired();

            // modelBuilder.Entity<JobSurveillance>()
            //     .OwnsOne(job => job.Contact, contact =>
            //     {
            //         contact.Property(vo => vo.Name).IsRequired();
            //         contact.Property(vo => vo.PhoneNumber).IsRequired();
            //     }
            //     ).Navigation(job => job.Contact).IsRequired();

            modelBuilder.Entity<Job>()
                .OwnsOne(job => job.Location, locationBuilder =>
                {
                    locationBuilder.OwnsOne(location => location.StartingPoint);
                    locationBuilder.OwnsOne(location => location.EndingPoint);
                    locationBuilder.Navigation(location => location.StartingPoint).IsRequired();
                    locationBuilder.Navigation(location => location.EndingPoint).IsRequired();
                }).Navigation(job => job.Location).IsRequired();


            // modelBuilder.Entity<Blog>()
            //     .HasDiscriminator<string>("blog_type")
            //     .HasValue<Blog>("blog_base")
            //     .HasValue<Blog>("blog_base");

            // modelBuilder.Entity<Family>()
            //     .HasDiscriminator<string>("fam_type")
            //     .HasValue<Family>("fam")
            //     .HasValue<MomFamily>("mom_fam")
            //     .HasValue<DadFamily>("dad_fam");
        }

        private void ConfigureContact<TEntity>(EntityTypeBuilder<TEntity> builder,
                                               Expression<Func<TEntity, JobContact>> navigationExpression)
            where TEntity : class
        {
            builder.OwnsOne(navigationExpression, contact =>
            {
                contact.Property(vo => vo.Name).IsRequired();
                contact.Property(vo => vo.PhoneNumber).IsRequired();
            }).Navigation(navigationExpression).IsRequired();
        }

    }
    // public class Blog : Entity<BlogId>, IAggregateRoot
    // {
    //     public string Url { get; set; }
    //     public Blog()
    //     {
    //         this.Id = new BlogId(Guid.NewGuid());
    //     }
    // }
    // public class RssBlog : Blog
    // {
    //     private string RssUrl { get; set; }
    //     private string newName { get; set; }
    //     public RssBlog() : base() { }
    // }
    // public class BlogId : EntityId
    // {
    //     [JsonConstructor]
    //     public BlogId(Guid value) : base(value)
    //     {
    //     }

    //     public BlogId(String value) : base(value)
    //     {
    //     }

    //     override
    //     protected Object createFromString(String text)
    //     {
    //         return new Guid(text);
    //     }

    //     override
    //     public String AsString()
    //     {
    //         Guid obj = (Guid)base.ObjValue;
    //         return obj.ToString();
    //     }

    //     public Guid AsGuid()
    //     {
    //         return (Guid)base.ObjValue;
    //     }
    // }
    // internal class BlogEntityTypeConfiguration : IEntityTypeConfiguration<Blog>
    // {
    //     public void Configure(EntityTypeBuilder<Blog> builder)
    //     {
    //         builder.HasKey(b => b.Id);
    //         builder.Property(b => b.Id)
    //                .HasConversion(
    //                    id => id.AsGuid(), // Convert from JobId to Guid
    //                    guid => new BlogId(guid.ToString()) // Convert from Guid to JobId
    //                );

    //     }
    // }
}