using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Categories;

namespace DDDSample1.Domain.Products
{
    public class Product : Entity<ProductId>, IAggregateRoot
    {
        public string Description { get;  private set; }

        public CategoryId CategoryId { get;  private set; }
        public bool Active{ get;  private set; }

        private Product()
        {
            this.Active = true;
        }

        public Product(string description, CategoryId catId)
        {
            if (catId == null)
                throw new BusinessRuleValidationException("Every product requires a category.");
            this.Id = new ProductId(Guid.NewGuid());
            this.Description = description;
            this.CategoryId = catId;
            this.Active = true;
        }

        public void ChangeDescription(string description)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the description to an inactive product.");
            this.Description = description;
        }

        public void ChangeCategoryId(CategoryId catId)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the category of an inactive product.");
            if (catId == null)
                throw new BusinessRuleValidationException("Every product requires a category.");
            this.CategoryId = catId;;
        }
        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}