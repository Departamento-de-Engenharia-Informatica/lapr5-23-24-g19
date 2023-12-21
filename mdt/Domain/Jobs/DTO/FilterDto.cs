namespace DDDSample1.Domain.Jobs
{
#nullable enable

    public class FilterDTO
    {
        public string Filter { get; set; }
        public string? Email { get; set; }
        public int? State { get; set; }
        //TODO: LIST OF TYPES?
        public int? Type { get; set; }


        public FilterDTO(string filter, string? email, int? state, int? type)
        {
            Filter = filter;
            Email = email;
            Type = type;
            State = state;
        }
    }
}
