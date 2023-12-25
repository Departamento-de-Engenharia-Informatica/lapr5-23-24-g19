using System;
using System.Linq.Expressions;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Sequences;
using DDDSample1.Infrastructure.Jobs;
using DDDSample1.Infrastructure.Sequences;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure
{
    public class RobDroneDBContext : DbContext
    {
        public DbSet<Job> Jobs { get; set; }
        public DbSet<Sequence> Sequences { get; set; }

        // public DbSet<JobSurveillance> surv { get; set; }
        // public DbSet<JobDelivery> delv { get; set; }

        public RobDroneDBContext(DbContextOptions options)
            : base(options) { }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new JobOrderEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new SequenceEntityTypeConfiguration());

            modelBuilder.Entity<Sequence>().OwnsOne(sequence => sequence.RobotPosition);

            modelBuilder.ApplyConfiguration(new JobEntityTypeConfiguration());

            modelBuilder
                .Entity<Job>()
                .HasDiscriminator<JobTypeEnum>("JobType") // Or another appropriate discriminator
                .HasValue<JobDelivery>(JobTypeEnum.DELIVERY)
                .HasValue<JobSurveillance>(JobTypeEnum.SURVEILLANCE);

            ConfigureContact(modelBuilder.Entity<JobDelivery>(), job => job.PickupContact);
            ConfigureContact(modelBuilder.Entity<JobDelivery>(), job => job.DeliveryContact);
            ConfigureContact(
                modelBuilder.Entity<JobSurveillance>(),
                job => job.SurveillanceContact
            );

            modelBuilder
                .Entity<Job>()
                .OwnsOne(
                    job => job.Location,
                    locationBuilder =>
                    {
                        locationBuilder.OwnsOne(location => location.StartingPoint);
                        locationBuilder.OwnsOne(location => location.EndingPoint);
                        locationBuilder.Navigation(location => location.StartingPoint).IsRequired();
                        locationBuilder.Navigation(location => location.EndingPoint).IsRequired();
                    }
                )
                .Navigation(job => job.Location)
                .IsRequired();

            modelBuilder
                .Entity<JobDelivery>()
                .OwnsOne(
                    job => job.ConfirmationCode,
                    job =>
                    {
                        job.Property(vo => vo.Code).IsRequired();
                    }
                );
        }

        private void ConfigureContact<TEntity>(
            EntityTypeBuilder<TEntity> builder,
            Expression<Func<TEntity, JobContact>> navigationExpression
        )
            where TEntity : class
        {
            builder
                .OwnsOne(
                    navigationExpression,
                    contact =>
                    {
                        contact.Property(vo => vo.Name).IsRequired();
                        contact.Property(vo => vo.PhoneNumber).IsRequired();
                    }
                )
                .Navigation(navigationExpression)
                .IsRequired();
        }
    }
}
