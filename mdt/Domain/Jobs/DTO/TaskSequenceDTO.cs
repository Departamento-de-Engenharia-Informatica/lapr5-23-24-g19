using System.Collections.Generic;

namespace DDDSample1.Domain.Jobs.DTO
{
    public class Position
    {
        public string building { get; set; }
        public int floor { get; set; }
        public int x { get; set; }
        public int y { get; set; }
    }

    public class TaskUnit
    {
        public Position start { get; set; }
        public Position end { get; set; }
        public string taskId { get; set; }
    }

    public class TaskSequenceDto
    {
        public double cost { get; set; }
        public Position initialPosition { get; set; }
        public List<TaskUnit> order { get; set; }
    }
}
