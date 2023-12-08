using DDDSample1.Domain.Families;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Families
{
    public class FamilyRepository : BaseRepository<Family, FamilyId>, IFamilyRepository
    {
      
        public FamilyRepository(DDDSample1DbContext context):base(context.Families)
        {
            
        }

    }
}