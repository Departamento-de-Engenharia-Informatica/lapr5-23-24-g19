using DDDSample1.Domain.Shared;
using System;
namespace DDDSample1.Domain.Families
{
    public class Family : Entity<FamilyId>, IAggregateRoot
    {

        public string Description { get;  private set; }


        public Family()
        {
            this.Id = new FamilyId(Guid.NewGuid());
        }

    }
}