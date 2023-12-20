using System;
using System.IO;

namespace DDDSample1.Domain.Jobs
{
#nullable enable

    public static class FilterMapper
    {

        public static FilterDTO ToDto(string filter, string rule)
        {
            Console.WriteLine(rule);

            switch (filter)
            {
                case "STATE":
                    if (int.TryParse(rule, out int state))
                    {
                        return new FilterDTO(filter, null, state, null);
                    }
                    break;
                //TODO: TYPE OR LIST OF TYPES??
                case "TYPE":
                    if (int.TryParse(rule, out int type))
                    {
                        return new FilterDTO(filter, null, null, type);
                    };
                    break;
                case "CLIENT":
                    if (rule.Length > 0)
                    {
                        return new FilterDTO(filter, rule, null, null);
                    }
                    break;
            }
            return null;
        }
        // public string Filter {get; private set;}
        // public string? Email {get; private set;}
        // public int? State {get; private set;}
        // public int? Type {get; private set;}
        // public FilterDTO(string filter,string? Email,int? state,int? type)
        // {
        //     this.Filter = filter;
        //     this.Email = Email;
        //     this.Type = type;
        //     this.State = state;

        // }
    }
}