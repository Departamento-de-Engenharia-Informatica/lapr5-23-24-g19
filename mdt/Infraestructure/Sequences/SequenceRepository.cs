using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Sequences;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Sequences
{
    public class SequenceRepository : BaseRepository<Sequence, SequenceId>, ISequenceRepository
    {
        public SequenceRepository(RobDroneDBContext context)
            : base(context.Sequences) { }

        private List<JobOrder> SortJobs(List<JobOrder> jobs)
        {
            return jobs.OrderBy(y => y.Order).ToList();
        }

        public async Task<Sequence> GetById(string id)
        {
            var sequence = await _objs.FindAsync(new SequenceId(id));

            if (sequence != null)
            {
                sequence.Jobs = SortJobs(sequence.Jobs);
            }

            return sequence;
        }

        public async Task<List<Sequence>> GetByRobotName(string robotName)
        {
            var sequences = await _objs.Where(x => robotName.Equals(x.RobotName)).ToListAsync();

            foreach (var s in sequences)
            {
                s.Jobs = SortJobs(s.Jobs);
            }

            return sequences;
        }
    }
}
