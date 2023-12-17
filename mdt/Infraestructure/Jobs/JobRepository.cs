using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Products;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Families
{
    public class JobRepository : BaseRepository<Job, JobId>, IJobRepository
    {
      
        public JobRepository(RobDroneDBContext context):base(context.Jobs)
        {
            
        }
    }
}