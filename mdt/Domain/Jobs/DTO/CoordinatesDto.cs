using System;

namespace DDDSample1.Domain.Jobs
{
    public class CoordinatesDto
    {
        public string BuildingCode { get; private set; }
        public int FloorNumber { get; private set; }
        public float X { get; private set; }
        public float Y { get; private set; }


        public CoordinatesDto(string BuildingCode, int FloorNumber, float x, float y)
        {
            this.BuildingCode = BuildingCode;
            this.FloorNumber = FloorNumber;
            this.X = x;
            this.Y = y;
        }
    }
}