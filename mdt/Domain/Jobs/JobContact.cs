using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public interface IJobContactProps
    {
        string Name { get; set; }
        int PhoneNumber { get; set; }
    }

    public class JobContact : IValueObject
    {
        public string Name { get; private set; }
        public int PhoneNumber { get; private set; }
        public JobContact(){}

        public JobContact(string name, int phoneNumber)
        {
            if (name == null)
                throw new BusinessRuleValidationException("Name is required.");
            if (phoneNumber.ToString().Length != 9)
                throw new BusinessRuleValidationException(
                    "Phone number should be 9 characters long."
                );

            this.Name = name;
            this.PhoneNumber = phoneNumber;
        }
    }
}
