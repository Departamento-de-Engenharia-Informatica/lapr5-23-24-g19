using System;
// using System.Globalization;
using System.Linq.Expressions;

namespace DDDSample1.Domain.Jobs.Filter
{
    public class JobStateFilterStrategy : IJobFilterStrategy
    {
        private readonly JobStateEnum state;

        public JobStateFilterStrategy(int state)
        {
            // int status = int.Parse(state, CultureInfo.InvariantCulture);
            this.state = JobState.FromCode(state);
        }

        public Expression<Func<Job, bool>> GetFilterExpression()
        {
            return j => j.Status == state;
        }
    }

}
