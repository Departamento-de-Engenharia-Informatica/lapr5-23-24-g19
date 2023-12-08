using System;

namespace DDDSample1.Domain.Jobs
{
    public class JobDto
    {
        public Guid Id { get; set; }

        public JobDto(Guid Id)
        {
            this.Id = Id;
        }
    }
}