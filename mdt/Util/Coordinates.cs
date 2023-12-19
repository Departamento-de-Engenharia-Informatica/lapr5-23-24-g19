using DDDSample1.Domain.Shared;

namespace DDDSample1.Util.Coordinates
{
    public class Coordinates
    {
        // TODO: maybe we should be using the domain ids here
        public string BuildingCode { get; private set; }
        public int FloorNumber { get; private set; }
        public float X { get; private set; }
        public float Y { get; private set; }

        private Coordinates(){}

        public Coordinates(string BuildingCode, int FloorNumber, float x, float y)
        {
            if (BuildingCode == null)
                throw new BusinessRuleValidationException("Building code is required.");
                
            this.BuildingCode = BuildingCode;
            this.FloorNumber = FloorNumber;
            this.X = x;
            this.Y = y;
        }
    }
}
