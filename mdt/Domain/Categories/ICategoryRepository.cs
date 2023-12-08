
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Categories
{
    public interface ICategoryRepository: IRepository<Category, CategoryId>
    {
    }
}