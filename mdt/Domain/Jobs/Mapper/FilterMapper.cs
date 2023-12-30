using System;
using System.Globalization;

namespace DDDSample1.Domain.Jobs
{
    public static class FilterMapper
    {

        public static FilterDTO ToDto(string filter, string rule)
        {
            Console.WriteLine("========================================");
            Console.WriteLine($"filter={filter} | rule={rule}");
            Console.WriteLine("========================================");


            return filter.ToUpper(CultureInfo.InvariantCulture) switch
            {
                "STATE" when int.TryParse(rule, out int state) => new FilterDTO(filter, null, state, null),
                "TYPE" when int.TryParse(rule, out int type) => new FilterDTO(filter, null, null, type),
                "CLIENT" => new FilterDTO(filter, rule, null, null),
                _ => throw new ArgumentException($"Invalid filter/rule: {filter}/{rule}"),
            };
        }
    }
}
