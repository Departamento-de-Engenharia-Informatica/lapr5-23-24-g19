using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Jobs.Filter;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Jobs
{
    public class JobRepository : BaseRepository<Job, JobId>, IJobRepository
    {
        public JobRepository(RobDroneDBContext context)
            : base(context.Jobs) { }

        public async Task<List<Job>> GetByType(JobTypeEnum type)
        {
            return await _objs.Where(x => type.Equals(x.JobType)).ToListAsync();
            ;
        }

        public async Task<List<Job>> GetByTypes(JobTypeEnum[] types)
        {
            return await _objs.Where(x => types.Contains(x.JobType)).ToListAsync();
        }

        public async Task<List<Job>> GetByEmail(string email)
        {
            return await _objs.Where(x => email.Equals(x.Email)).ToListAsync();
        }

        public async Task<List<Job>> GetByState(JobStateEnum state)
        {
            return await _objs.Where(x => state.Equals(x.Status)).ToListAsync();
        }

        public Task<List<Job>> Filter(IJobFilterStrategy strategy)
        {
            var filter = strategy.GetFilterExpression();
            return _objs.Where(filter).ToListAsync();
        }
    }
}
