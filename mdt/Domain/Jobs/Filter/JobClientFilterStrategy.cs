
using System;
using System.Linq.Expressions;

namespace DDDSample1.Domain.Jobs.Filter
{
    public class JobClientFilterStrategy : IJobFilterStrategy
    {
        private readonly string email;

        public JobClientFilterStrategy(string email)
        {
            this.email = email;
        }

        public Expression<Func<Job, bool>> GetFilterExpression()
        {
            return j => j.Email == email;
        }
    }

}
