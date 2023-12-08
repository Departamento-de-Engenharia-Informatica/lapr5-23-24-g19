using DDDSample1.Domain.Products;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Products
{
    public class ProductRepository : BaseRepository<Product, ProductId>,IProductRepository
    {
        public ProductRepository(DDDSample1DbContext context):base(context.Products)
        {
           
        }
    }
}