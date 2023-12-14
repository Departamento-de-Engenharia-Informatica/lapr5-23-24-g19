namespace DDDSample1.Domain.Jobs
{
    public class JobContactDto
    {
        public string Name { get; private set; }
        public int PhoneNumber { get; private set; }

        public JobContactDto(string Name, int PhoneNumber)
        {
            this.Name = Name;
            this.PhoneNumber = PhoneNumber;
        }
    }
}