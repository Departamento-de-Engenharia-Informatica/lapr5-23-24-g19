using DDDSample1.Domain.Jobs.DTO;

namespace DDDSample1.Domain.Jobs
{
    public class JobDelivery : Job
    {
        public JobContact PickupContact { get; set; }
        public JobContact DeliveryContact { get; set; }
        public JobConfirmationCode ConfirmationCode { get; set; }
        public string Description { get; set; }

        protected JobDelivery()
            : base() { }

        public JobDelivery(
            string email,
            JobLocation location,
            JobContact pickupContact,
            JobContact deliveryContact,
            JobConfirmationCode confirmationCode,
            string description
        )
            : base(email, location, JobTypeEnum.DELIVERY)
        {
            this.PickupContact = pickupContact;
            this.DeliveryContact = deliveryContact;
            this.ConfirmationCode = confirmationCode;
            this.Description = description;
        }

        protected override Job InternalUpdate(JobUpdateProps updateInfo) => this;
    }
}
