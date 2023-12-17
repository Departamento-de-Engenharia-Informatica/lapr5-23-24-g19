using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Families;

namespace DDDSample1.Infrastructure.Categories
{
    internal class FamilyEntityTypeConfiguration : IEntityTypeConfiguration<Family>
    {
        public void Configure(EntityTypeBuilder<Family> builder)
        {
            builder.HasKey(b => b.Id);
            builder.Property(b => b.Id)
                   .HasConversion(
                       id => id.AsGuid(), // Convert from JobId to Guid
                       guid => new FamilyId(guid.ToString()) // Convert from Guid to JobId
                   );

        }
    }
}