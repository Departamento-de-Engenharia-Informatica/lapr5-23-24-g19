using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Jobs.DTO;

namespace DDDSample1.Domain.Jobs.Mapper
{
    public class ComputeSequenceMapper
    {
        public ComputeSequenceMapper() { }

        public static ComputeSequenceDto ToDTO(string algorithm, List<Job> jobs)
        {
            var units = jobs.Select(j =>
                {
                    var start = new JobPosition
                    {
                        building = j.Location.StartingPoint.BuildingCode,
                        floor = j.Location.StartingPoint.FloorNumber,
                        // This is not a bug. The x and y are inverted in the DTO.
                        x = (int)j.Location.StartingPoint.Y,
                        y = (int)j.Location.StartingPoint.X
                    };

                    var end = new JobPosition
                    {
                        building = j.Location.EndingPoint.BuildingCode,
                        floor = j.Location.EndingPoint.FloorNumber,
                        // This is not a bug. The x and y are inverted in the DTO.
                        x = (int)j.Location.EndingPoint.Y,
                        y = (int)j.Location.EndingPoint.X
                    };

                    return new JobTaskUnit
                    {
                        start = start,
                        end = end,
                        taskId = j.Id.Value
                    };
                })
                .ToList();

            return new ComputeSequenceDto { algorithm = algorithm, tasks = units };
        }
    }
}
