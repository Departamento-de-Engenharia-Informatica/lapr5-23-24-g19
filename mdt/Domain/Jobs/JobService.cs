using DDDSample1.Domain.Products;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public class JobService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IJobRepository _repo;

        public JobService(IUnitOfWork unitOfWork, IJobRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }
    }    
}