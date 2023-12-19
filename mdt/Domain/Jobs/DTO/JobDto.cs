namespace DDDSample1.Domain.Jobs
{
    #nullable enable

    public class JobDto
    {
        public string Email { get; private set; }
        public JobLocationDto Location { get; private set; }
        public int Type { get; private set; }

        public JobDeliveryDto? Delivery { get; private set; }
        public JobSurveillanceDto? Surveillance { get; private set; }

        public JobDto(string Email, JobLocationDto Location, int Type, JobDeliveryDto? Delivery, JobSurveillanceDto? Surveillance)
        {
            this.Email = Email;
            this.Location = Location;
            this.Type = Type;
            
            this.Delivery = Delivery;
            this.Surveillance = Surveillance;
        }
    }
}