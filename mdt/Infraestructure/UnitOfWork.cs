using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Infrastructure
{
    public class UnitOfWork : IUnitOfWork
    {
        private readonly DDDSample1DbContext _context;

        public UnitOfWork(DDDSample1DbContext context)
        {
            this._context = context;
        }

        public async Task<int> CommitAsync()
        {
            return await this._context.SaveChangesAsync();
        }
    }
}