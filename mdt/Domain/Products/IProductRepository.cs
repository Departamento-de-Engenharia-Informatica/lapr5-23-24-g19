using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Products
{
    public interface IProductRepository: IRepository<Product,ProductId>
    {
    }
}