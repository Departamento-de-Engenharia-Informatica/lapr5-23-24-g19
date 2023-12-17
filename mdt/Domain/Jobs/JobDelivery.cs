using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public class JobDelivery: Job, IAggregateRoot
    {
        private JobContact PickupContact { get; set; }
        private JobContact DeliveryContact { get; set; }
        private string Description { get; set; }

        protected JobDelivery():base(){

        }
        public JobDelivery(string email, JobLocation location, JobContact pickupContact, JobContact deliveryContact, string description) : base(email, location, JobTypeEnum.DELIVERY)
        {
            this.PickupContact = pickupContact;
            this.DeliveryContact = deliveryContact;
            this.Description = description;
        }
    }
}