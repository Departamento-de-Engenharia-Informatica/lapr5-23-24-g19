using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;

namespace DDDSample1.Domain.Sequences
{
    public class JobOrder : Entity<JobOrderId>, IAggregateRoot
    {
        public Job Job { get; private set; }
        public int Order { get; private set; }

        public JobOrder() { }

        public JobOrder(Job job, int order)
        {
            Id = new JobOrderId(Guid.NewGuid());
            Job = job;
            Order = order;
        }
    }

    public class Sequence : Entity<SequenceId>, IAggregateRoot
    {
        public List<JobOrder> Jobs { get; set; }
        public double Cost { get; private set; }
        public string RobotName { get; private set; }
        public Coordinates RobotPosition { get; private set; }

        public Sequence() { }

        public Sequence(List<Job> jobs, double cost, string robotName, Coordinates robotPosition)
        {
            Id = new SequenceId(Guid.NewGuid());
            Jobs = jobs.Select((job, index) => new JobOrder(job, index)).ToList();
            Cost = cost;
            RobotName = robotName;
            RobotPosition = robotPosition;
        }
    }
}
