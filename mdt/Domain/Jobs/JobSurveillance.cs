using DDDSample1.Domain.Jobs.DTO;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public class JobSurveillance : Job, IAggregateRoot
    {
        public JobContact SurveillanceContact { get; set; }

        protected JobSurveillance()
            : base() { }

        public JobSurveillance(string email, JobLocation location, JobContact contact)
            : base(email, location, JobTypeEnum.SURVEILLANCE)
        {
            this.SurveillanceContact = contact;
        }

        protected override Job InternalUpdate(JobUpdateProps updateInfo) => this;
    }
}
