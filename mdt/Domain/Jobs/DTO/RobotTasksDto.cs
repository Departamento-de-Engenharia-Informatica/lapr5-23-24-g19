using System.Collections.Generic;

namespace DDDSample1.Domain.Jobs.DTO
{
    public class TaskIdsDTO
    {
        public string id { get; set; }
        public string type { get; set; }
    }

    public class RobotTasksDTO
    {
        public string Algorithm { get; set; }
        public Dictionary<string, List<TaskIdsDTO>> RobotTasks { get; set; }
    }
}
