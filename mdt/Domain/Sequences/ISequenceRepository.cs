using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Sequences
{
    public interface ISequenceRepository : IRepository<Sequence, SequenceId>
    {
        public Task<Sequence> GetById(string id);
        public Task<List<Sequence>> GetByRobotName(string robotName);
    }
}
