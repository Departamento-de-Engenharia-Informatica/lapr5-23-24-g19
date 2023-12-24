using System.Collections.Generic;

namespace DDDSample1.Domain.Jobs.DTO
{
    public class PlannedTasksDTO
    {
        public Dictionary<string, TaskSequenceDto> RobotTasks { get; set; }
    }
}
