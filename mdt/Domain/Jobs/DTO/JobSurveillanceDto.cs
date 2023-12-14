namespace DDDSample1.Domain.Jobs
{
    public class JobSurveillanceDto
    {
        public JobContactDto Contact { get; private set; }

        public JobSurveillanceDto(JobContactDto Contact)
        {
            this.Contact = Contact;
        }
    }
}