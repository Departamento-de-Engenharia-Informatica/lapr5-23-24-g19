using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Products;

namespace DDDSample1.Infrastructure.Products
{
    internal class ProductEntityTypeConfiguration : IEntityTypeConfiguration<Product>
    {
        public void Configure(EntityTypeBuilder<Product> builder)
        {
            //builder.ToTable("Products", SchemaNames.DDDSample1);
            builder.HasKey(b => b.Id);
        }
    }
}