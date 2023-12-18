namespace DDDSample1.Domain.Jobs
{
    public class JobDeliveryDto
    {
        public JobContactDto PickupContact { get; private set; }
        public JobContactDto DeliveryContact { get; private set; }
        public string Description { get; private set; }
        public int ConfirmationCode { get; private set; }

        public JobDeliveryDto(
            JobContactDto PickupContact,
            JobContactDto DeliveryContact,
            string Description,
            int ConfirmationCode
        )
        {
            this.PickupContact = PickupContact;
            this.DeliveryContact = DeliveryContact;
            this.Description = Description;
            this.ConfirmationCode = ConfirmationCode;
        }
    }
}
