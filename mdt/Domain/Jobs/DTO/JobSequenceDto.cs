using System.Collections.Generic;

namespace DDDSample1.Domain.Jobs.DTO
{
    public class JobSequenceDto
    {
        public double cost { get; set; }
        public Position initialPosition { get; set; }
        public List<Job> order { get; set; }
    }
}
