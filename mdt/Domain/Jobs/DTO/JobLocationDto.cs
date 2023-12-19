using System;

namespace DDDSample1.Domain.Jobs
{
    public class JobLocationDto
    {
        public CoordinatesDto StartingPoint { get; private set; }
        public CoordinatesDto EndingPoint { get; private set; }

        public JobLocationDto(CoordinatesDto StartingPoint, CoordinatesDto EndingPoint)
        {
            this.StartingPoint = StartingPoint;
            this.EndingPoint = EndingPoint;
        }
    }
}