using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Jobs
{
    public abstract class Job : Entity<JobId>, IAggregateRoot
    {
        //atribuido pelo mdr e nao pelo spa
        public string Email { get; private set; }
        public JobLocation Location { get; private set; }
        public JobStateEnum Status { get; private set; }
        public JobTypeEnum JobType { get; private set; }

        // Pessoa que pede a tarefa
        // Para vigilancia, tem de ser indicado o edificio, os pisos e o contacto
        // Para entrega tem de ser indicada a sala de levantamento, sala de entrega, nome e contacto para entrega e levantamento + Descricao de entrega
        public Job()
        {
        }

        public Job(string Email, JobLocation location, JobTypeEnum type)
        {
            this.Id = new JobId(Guid.NewGuid());
            this.Email = Email;
            this.Location = location;
            this.Status = JobStateEnum.PENDING;
            this.JobType = type;
        }
    }
}
