using System;

namespace DDDSample1.Domain.Jobs
{
    #nullable enable

    public class CreatingJobDto
    {
        public string Email { get; private set; }
        public JobLocationDto Location { get; private set; }
        public int JobType { get; private set; }
        public JobDeliveryDto? Delivery { get; private set; }
        public JobSurveillanceDto? Surveillance { get; private set; }

        public CreatingJobDto(string Email, JobLocationDto Location, int JobType, JobDeliveryDto? Delivery, JobSurveillanceDto? Surveillance)
        {
            this.Email = Email;
            this.Location = Location;
            this.JobType = JobType;
            if(Surveillance!=null){
                this.Surveillance = Surveillance;
            }
            if(Delivery!=null){
                this.Delivery = Delivery;
            }
        }
    }
}