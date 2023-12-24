using DDDSample1.Domain.Sequences;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure.Sequences
{
    internal class JobOrderEntityTypeConfiguration : IEntityTypeConfiguration<JobOrder>
    {
        public void Configure(EntityTypeBuilder<JobOrder> builder)
        {
            builder.HasKey(b => b.Id);
            builder
                .Property(b => b.Id)
                .HasConversion(id => id.AsGuid(), guid => new JobOrderId(guid.ToString()));
        }
    }
}
