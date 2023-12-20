namespace DDDSample1.Domain.Jobs
{
    #nullable enable

    public class FilterDTO
    {
        public string Filter {get; set;}
        public string? Email {get; set;}
        public int? State {get; set;}
        //TODO: LIST OF TYPES?
        public int? Type {get; set;}
        public FilterDTO(string filter,string? Email,int? state,int? type)
        {
            this.Filter = filter;
            this.Email = Email;
            this.Type = type;
            this.State = state;
            
        }
    }
}