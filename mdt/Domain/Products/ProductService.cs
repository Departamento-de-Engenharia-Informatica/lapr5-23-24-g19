using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Categories;

namespace DDDSample1.Domain.Products
{
    public class ProductService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IProductRepository _repo;

        private readonly ICategoryRepository _repoCat;

        public ProductService(IUnitOfWork unitOfWork, IProductRepository repo, ICategoryRepository repoCategories)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._repoCat = repoCategories;
        }

        public async Task<List<ProductDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<ProductDto> listDto = list.ConvertAll<ProductDto>(prod => 
                new ProductDto(prod.Id.AsGuid(),prod.Description,prod.CategoryId));

            return listDto;
        }

        public async Task<ProductDto> GetByIdAsync(ProductId id)
        {
            var prod = await this._repo.GetByIdAsync(id);
            
            if(prod == null)
                return null;

            return new ProductDto(prod.Id.AsGuid(),prod.Description,prod.CategoryId);
        }

        public async Task<ProductDto> AddAsync(CreatingProductDto dto)
        {
            await checkCategoryIdAsync(dto.CategoryId);
            var product = new Product(dto.Description,dto.CategoryId);

            await this._repo.AddAsync(product);

            await this._unitOfWork.CommitAsync();

            return new ProductDto(product.Id.AsGuid(),product.Description,product.CategoryId);
        }

        public async Task<ProductDto> UpdateAsync(ProductDto dto)
        {
            await checkCategoryIdAsync(dto.CategoryId);
            var product = await this._repo.GetByIdAsync(new ProductId(dto.Id)); 

            if (product == null)
                return null;   

            // change all fields
            product.ChangeDescription(dto.Description);
            product.ChangeCategoryId(dto.CategoryId);
            
            await this._unitOfWork.CommitAsync();

            return new ProductDto(product.Id.AsGuid(),product.Description,product.CategoryId);
        }

        public async Task<ProductDto> InactivateAsync(ProductId id)
        {
            var product = await this._repo.GetByIdAsync(id); 

            if (product == null)
                return null;   

            product.MarkAsInative();
            
            await this._unitOfWork.CommitAsync();

            return new ProductDto(product.Id.AsGuid(),product.Description,product.CategoryId);
        }

        public async Task<ProductDto> DeleteAsync(ProductId id)
        {
            var product = await this._repo.GetByIdAsync(id); 

            if (product == null)
                return null;   

            if (product.Active)
                throw new BusinessRuleValidationException("It is not possible to delete an active product.");
            
            this._repo.Remove(product);
            await this._unitOfWork.CommitAsync();

            return new ProductDto(product.Id.AsGuid(),product.Description,product.CategoryId);
        }

        private async Task checkCategoryIdAsync(CategoryId categoryId)
        {
           var category = await _repoCat.GetByIdAsync(categoryId);
           if (category == null)
                throw new BusinessRuleValidationException("Invalid Category Id.");
        }
    }
}