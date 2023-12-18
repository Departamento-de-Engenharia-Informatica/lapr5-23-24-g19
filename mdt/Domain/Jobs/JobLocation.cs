using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;

namespace DDDSample1.Domain.Jobs
{
    public class JobLocation : IValueObject
    {
        public Coordinates StartingPoint { get; private set; }
        public Coordinates EndingPoint { get; private set; }

        private JobLocation() { }

        public JobLocation(Coordinates startingPoint, Coordinates endingPoint)
        {
            this.StartingPoint =
                startingPoint
                ?? throw new BusinessRuleValidationException("Starting point is required.");
            this.EndingPoint =
                endingPoint
                ?? throw new BusinessRuleValidationException("Ending point is required.");
        }
    }
}
