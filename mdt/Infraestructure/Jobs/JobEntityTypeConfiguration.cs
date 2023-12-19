using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Jobs;

namespace DDDSample1.Infrastructure.Jobs
{
    internal class JobEntityTypeConfiguration : IEntityTypeConfiguration<Job>
    {
        public void Configure(EntityTypeBuilder<Job> builder)
        {
            builder.HasKey(b => b.Id);
            builder.Property(b => b.Id)
                   .HasConversion(
                       id => id.AsGuid(), // Convert from JobId to Guid
                       guid => new JobId(guid.ToString()) // Convert from Guid to JobId
                   );
                   
        }
    }
}
