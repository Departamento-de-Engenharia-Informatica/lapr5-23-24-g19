using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;
using DDDSample1.Domain.Jobs.DTO;
using DDDSample1.Domain.Jobs.Filter;
using DDDSample1.Domain.Jobs.Mapper;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Sequences;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure.Jobs;
using DDDSample1.Util.Coordinates;

namespace DDDSample1.Domain.Jobs
{
    public class SequenceService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IJobRepository _repo;
        private readonly PlanningAdapter _planning;
        private readonly ISequenceRepository _sequenceRepo;

        public SequenceService(
            IUnitOfWork unitOfWork,
            IJobRepository repo,
            PlanningAdapter planning,
            ISequenceRepository sequenceRepo
        )
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._planning = planning;
            this._sequenceRepo = sequenceRepo;
        }
        public async Task<List<Sequence>> RobotSequence(string name)
        {
            Console.WriteLine("hello2");
            var seq = await _sequenceRepo.GetByRobotName(name);
            var options = new JsonSerializerOptions
            {
                WriteIndented = true // This sets the indentation
            };

            
            Console.WriteLine("Seqs:\n\n",JsonSerializer.Serialize(seq, options));

            if (seq == null)
            {
                throw new NotFoundException($"No seq found for {name}");
            }

            // TODO: DTO
            return seq;
            
        }
    }
}
