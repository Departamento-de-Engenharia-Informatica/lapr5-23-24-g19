using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public abstract class Job : Entity<JobId>, IAggregateRoot
    {
        public string Email { get; private set; }
        public JobLocation Location { get; private set; }
        public JobStateEnum Status { get; private set; }
        public JobTypeEnum JobType { get; private set; }

        public Job() { }

        public Job(string Email, JobLocation location, JobTypeEnum type)
        {
            this.Id = new JobId(Guid.NewGuid());
            this.Email = Email;
            this.Location = location;
            this.Status = JobStateEnum.PENDING;
            this.JobType = type;
        }
    }
}
