using System.Globalization;

namespace DDDSample1.Domain.Jobs.Filter
{
    public class JobFilterContext
    {
        public static IJobFilterStrategy FilterStrategy(FilterDTO filterDTO)
        {
            return filterDTO.Filter.ToUpper(CultureInfo.InvariantCulture) switch
            {
                "CLIENT" => new JobClientFilterStrategy(filterDTO.Email),
                "STATE" => new JobStateFilterStrategy(filterDTO.State ?? 0),
                "TYPE" => new JobDeviceFilterStrategy(filterDTO.Type ?? 0),
                _ => throw new System.NotImplementedException(),
            };
        }
    }

}
