namespace DDDSample1.Domain.Jobs
{
    public class JobContactDto
    {
        public string Name { get; private set; }
        public int Phone { get; private set; }

        public JobContactDto(string Name, int Phone)
        {
            this.Name = Name;
            this.Phone = Phone;
        }
    }
}