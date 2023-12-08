using DDDSample1.Domain.Categories;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Categories
{
    public class CategoryRepository : BaseRepository<Category, CategoryId>, ICategoryRepository
    {
    
        public CategoryRepository(DDDSample1DbContext context):base(context.Categories)
        {
           
        }


    }
}