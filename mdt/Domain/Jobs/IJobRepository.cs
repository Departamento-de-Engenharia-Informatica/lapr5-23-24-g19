using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Products
{
    public interface IJobRepository: IRepository<Job, JobId>
    {
    }
}