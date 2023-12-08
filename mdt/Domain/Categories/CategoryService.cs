using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Categories
{
    public class CategoryService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly ICategoryRepository _repo;

        public CategoryService(IUnitOfWork unitOfWork, ICategoryRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<CategoryDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();
            
            List<CategoryDto> listDto = list.ConvertAll<CategoryDto>(cat => new CategoryDto{Id = cat.Id.AsGuid(), Description = cat.Description});

            return listDto;
        }

        public async Task<CategoryDto> GetByIdAsync(CategoryId id)
        {
            var cat = await this._repo.GetByIdAsync(id);
            
            if(cat == null)
                return null;

            return new CategoryDto{Id = cat.Id.AsGuid(), Description = cat.Description};
        }

        public async Task<CategoryDto> AddAsync(CreatingCategoryDto dto)
        {
            var category = new Category(dto.Description);

            await this._repo.AddAsync(category);

            await this._unitOfWork.CommitAsync();

            return new CategoryDto { Id = category.Id.AsGuid(), Description = category.Description };
        }

        public async Task<CategoryDto> UpdateAsync(CategoryDto dto)
        {
            var category = await this._repo.GetByIdAsync(new CategoryId(dto.Id)); 

            if (category == null)
                return null;   

            // change all field
            category.ChangeDescription(dto.Description);
            
            await this._unitOfWork.CommitAsync();

            return new CategoryDto { Id = category.Id.AsGuid(), Description = category.Description };
        }

        public async Task<CategoryDto> InactivateAsync(CategoryId id)
        {
            var category = await this._repo.GetByIdAsync(id); 

            if (category == null)
                return null;   

            // change all fields
            category.MarkAsInative();
            
            await this._unitOfWork.CommitAsync();

            return new CategoryDto { Id = category.Id.AsGuid(), Description = category.Description };
        }

         public async Task<CategoryDto> DeleteAsync(CategoryId id)
        {
            var category = await this._repo.GetByIdAsync(id); 

            if (category == null)
                return null;   

            if (category.Active)
                throw new BusinessRuleValidationException("It is not possible to delete an active category.");
            
            this._repo.Remove(category);
            await this._unitOfWork.CommitAsync();

            return new CategoryDto { Id = category.Id.AsGuid(), Description = category.Description };
        }
    }
}