using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public class JobConfirmationCode : IValueObject
    {
        public int Code { get; private set; }

        public JobConfirmationCode(int code)
        {
            if (code.ToString().Length < 4 || code.ToString().Length > 6)
                throw new BusinessRuleValidationException(
                    "Confirmation code should be a number with 4 to 6 digits."
                );

            this.Code = code;
        }
    }
}
