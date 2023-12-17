using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public class JobDelivery: Job
    {
        public JobContact PickupContact { get; set; }
        // public string PickupContact { get; set; }
        public JobContact DeliveryContact { get; set; }
        // public string DeliveryContact { get; set; }
        public string Description { get; set; }

        protected JobDelivery():base(){

        }
        public JobDelivery(string email, JobLocation location, JobContact pickupContact, JobContact deliveryContact, string description) : base(email,location,  JobTypeEnum.DELIVERY)
        {
            this.PickupContact = pickupContact;
            this.DeliveryContact = deliveryContact;
            this.Description = description;
        }
    }
}