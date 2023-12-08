using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public class Job : Entity<JobId>, IAggregateRoot
    {
        public JobId TaskId { get; private set; }

        private Job()
        {
            // ...
        }

        // public Task(/* ... */)
        // {
        //     // ...
        // }

        // public void ChangeDescription(string description)
        // {
        //     if (!this.Active)
        //         throw new BusinessRuleValidationException(
        //             "It is not possible to change the description to an inactive product."
        //         );
        //     this.Description = description;
        // }

        // public void ChangeCategoryId(CategoryId catId)
        // {
        //     if (!this.Active)
        //         throw new BusinessRuleValidationException(
        //             "It is not possible to change the category of an inactive product."
        //         );
        //     if (catId == null)
        //         throw new BusinessRuleValidationException("Every product requires a category.");
        //     this.CategoryId = catId;
        //     ;
        // }

        // public void MarkAsInative()
        // {
        //     this.Active = false;
        // }
    }
}
