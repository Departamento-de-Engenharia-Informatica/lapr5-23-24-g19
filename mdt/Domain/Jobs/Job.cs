using System;
using DDDSample1.Domain.Jobs.DTO;
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
            Id = new JobId(Guid.NewGuid());
            this.Email = Email;
            Location = location;
            Status = JobStateEnum.PENDING;
            JobType = type;
        }

        public Job Update(JobUpdateProps updateInfo)
        {
            if (updateInfo.Status.HasValue)
            {
                UpdateStatus(updateInfo.Status.Value);
            }
            return InternalUpdate(updateInfo);
        }

        protected abstract Job InternalUpdate(JobUpdateProps updateInfo);

        private void UpdateStatus(JobStateEnum newStatus)
        {
            if (newStatus == Status)
            {
                return;
            }

            Status = (Status, newStatus) switch
            {
                (JobStateEnum.PENDING, JobStateEnum.APPROVED) => newStatus,
                (JobStateEnum.PENDING, JobStateEnum.REJECTED) => newStatus,
                (JobStateEnum.APPROVED, JobStateEnum.PLANNED) => newStatus,
                _ =>
                throw new BusinessRuleValidationException(
                    $"Task status cannot be altered from {JobState.ToString(Status)} to {JobState.ToString(newStatus)}"),
            };
        }
    }
}
