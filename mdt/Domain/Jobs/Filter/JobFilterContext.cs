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
                "STATE" => new JobStateFilterStrategy(filterDTO.State ?? 0 /* FIXME: .State is of int? type */),
                "DEVICE_TYPE" => throw new System.NotImplementedException(), // TODO
                _ => throw new System.NotImplementedException(),
            };
        }
    }

}
