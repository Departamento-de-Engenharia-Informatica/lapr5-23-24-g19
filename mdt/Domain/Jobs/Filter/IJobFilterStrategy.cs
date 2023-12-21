using System;
using System.Linq.Expressions;

namespace DDDSample1.Domain.Jobs.Filter
{
    public interface IJobFilterStrategy
    {
        Expression<Func<Job, bool>> GetFilterExpression();
    }
}
