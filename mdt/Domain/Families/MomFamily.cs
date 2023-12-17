using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Families
{
    public class MomFamily: Family
    {

        private string Mom { get;  set; }
        private string Contact {get; set;}


        private MomFamily(): base()
        {
        }

    }
}