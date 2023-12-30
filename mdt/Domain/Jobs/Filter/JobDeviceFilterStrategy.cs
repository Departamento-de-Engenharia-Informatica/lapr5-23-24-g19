using System;
using System.Linq.Expressions;

namespace DDDSample1.Domain.Jobs.Filter
{
    public class JobDeviceFilterStrategy : IJobFilterStrategy
    {
        // private readonly JobTypeEnum taskType;
        private readonly int taskType;

        public JobDeviceFilterStrategy(int taskType)
        {
            this.taskType = taskType;//JobType.FromCode(taskType);
        }

        public Expression<Func<Job, bool>> GetFilterExpression()
        {
            return j => ((int)j.JobType) == taskType;
        }
    }

}
