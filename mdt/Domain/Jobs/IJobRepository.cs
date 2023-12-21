using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Jobs.Filter;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Products
{
    public interface IJobRepository : IRepository<Job, JobId>
    {

        public Task<List<Job>> GetByType(JobTypeEnum type);
        public Task<List<Job>> GetByTypes(JobTypeEnum[] types);
        public Task<List<Job>> GetByEmail(string email);
        public Task<List<Job>> GetByState(JobStateEnum state);

        public Task<List<Job>> Filter(IJobFilterStrategy strategy);
    }
}
