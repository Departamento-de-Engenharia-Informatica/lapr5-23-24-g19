// using System.Collections.Generic;

namespace DDDSample1.Domain.Jobs.DTO
{
    public class PlannedRobotTasksDTO
    {
        public string RobotName { get; set; }
        public TaskSequenceDto Tasks { get; set; }
    }

    // public class PlannedTasksDTO
    // {
    //     public Dictionary<string, TaskSequenceDto> RobotTasks { get; set; }
    // }
}
