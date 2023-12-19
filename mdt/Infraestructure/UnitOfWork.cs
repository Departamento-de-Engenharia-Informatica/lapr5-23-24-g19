using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Infrastructure
{
    public class UnitOfWork : IUnitOfWork
    {
        private readonly RobDroneDBContext _context;

        public UnitOfWork(RobDroneDBContext context)
        {
            this._context = context;
        }

        public async Task<int> CommitAsync()
        {
            return await this._context.SaveChangesAsync();
        }
    }
}