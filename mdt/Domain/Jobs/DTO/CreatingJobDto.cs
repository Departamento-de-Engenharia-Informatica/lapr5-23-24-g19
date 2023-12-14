namespace DDDSample1.Domain.Jobs
{
    #nullable enable

    public class CreatingJobDto
    {
        public string Email { get; private set; }
        public JobLocationDto Location { get; private set; }
        public string Status { get; private set; }
        public int Type { get; private set; }

        public JobDeliveryDto? Delivery { get; private set; }
        public JobSurveillanceDto? Surveillance { get; private set; }

        public CreatingJobDto(string Email, JobLocationDto Location, string Status, int Type, JobDeliveryDto? Delivery, JobSurveillanceDto? Surveillance)
        {
            this.Email = Email;
            this.Location = Location;
            this.Status = Status;
            this.Type = Type;
            
            this.Delivery = Delivery;
            this.Surveillance = Surveillance;
        }
    }
}