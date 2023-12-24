using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Sequences;

namespace DDDSample1.Infrastructure.Sequences
{
    internal class SequenceEntityTypeConfiguration : IEntityTypeConfiguration<Sequence>
    {
        public void Configure(EntityTypeBuilder<Sequence> builder)
        {
            builder.HasKey(b => b.Id);
            builder.Property(b => b.Id)
                   .HasConversion(
                       id => id.AsGuid(),
                       guid => new SequenceId(guid.ToString())
                   );

        }
    }
}
