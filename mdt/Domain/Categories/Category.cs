using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Categories
{
    public class Category : Entity<CategoryId>, IAggregateRoot
    {
     
        public string Description { get;  private set; }

        public bool Active{ get;  private set; }

        private Category()
        {
            this.Active = true;
        }

        public Category(string description)
        {
            this.Id = new CategoryId(Guid.NewGuid());
            this.Description = description;
            this.Active = true;
        }

        public void ChangeDescription(string description)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the description to an inactive category.");
            this.Description = description;
        }
        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}