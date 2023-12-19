using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Families
{
    public class DadFamily: Family
    {

        public string Dad { get;  private set; }


        private DadFamily(): base()
        {
        }

    }
}