using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Products;
using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Linq;
using Microsoft.EntityFrameworkCore;
using System.Text.Json;

namespace DDDSample1.Infrastructure.Families
{
    public class JobRepository : BaseRepository<Job, JobId>, IJobRepository
    {
      
        public JobRepository(RobDroneDBContext context):base(context.Jobs)
        {
            
        }
        public async Task<List<Job>> GetByType(JobTypeEnum type){
            return await this._objs.Where(x =>type.Equals(x.JobType)).ToListAsync();;
        }

        public async Task<List<Job>> GetByTypes(JobTypeEnum[] types){
            return await this._objs.Where(x =>types.Contains(x.JobType)).ToListAsync();
        }
        
        public async Task<List<Job>> GetByEmail(string email){
            return await this._objs.Where(x =>email.Equals(x.Email)).ToListAsync();
        }
        public async Task<List<Job>> GetByState(JobStateEnum state){
            return await this._objs.Where(x => state.Equals(x.Status)).ToListAsync();
        }


    }
}