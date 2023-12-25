using System.Collections.Generic;

namespace DDDSample1.Domain.Jobs.DTO
{
    public class JobPosition
    {
        public string building { get; set; }
        public int floor { get; set; }
        public int x { get; set; }
        public int y { get; set; }
    }

    public class JobTaskUnit
    {
        public JobPosition start { get; set; }
        public JobPosition end { get; set; }
        public string taskId { get; set; }
    }

    public class ComputeSequenceDto
    {
        public string algorithm { get; set; }
        public List<JobTaskUnit> tasks { get; set; }
    }
}

