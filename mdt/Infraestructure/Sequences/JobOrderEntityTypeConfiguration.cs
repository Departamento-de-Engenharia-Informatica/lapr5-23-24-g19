using DDDSample1.Domain.Sequences;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure.Sequences
{
    internal class JobOrderEntityTypeConfiguration : IEntityTypeConfiguration<JobOrder>
    {
        public void Configure(EntityTypeBuilder<JobOrder> builder)
        {
                
            builder.HasKey(jo => jo.Id);
            builder
                .Property(b => b.Id)
                .HasConversion(id => id.AsGuid(), guid => new JobOrderId(guid.ToString()));

            builder.HasOne(jo => jo.Job)
                .WithMany()
                .HasForeignKey(jo => jo.JobId); // Configuring the foreign key to Job table

            builder.HasOne(jo => jo.Sequence)
                .WithMany(s => s.Jobs)
                .HasForeignKey(jo => jo.SequenceId); // Configuring the foreign key to Sequence table

            builder.Property(jo => jo.Order)
                .IsRequired();


        }
    }
}
