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
    public class JobService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IJobRepository _repo;
        private readonly PlanningAdapter _planning;
        private readonly ISequenceRepository _sequenceRepo;

        public JobService(
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

        public async Task<String> GetByIdAsync(String id)
        {
            var job = await this._repo.GetByIdAsync(new JobId(id));

            if (job == null)
                return null;

            var options = new JsonSerializerOptions
            {
                WriteIndented = true // This sets the indentation
            };

            return JsonSerializer.Serialize(job, options);
        }

        public async Task<CreatingJobDto> AddAsync(CreatingJobDto dto)
        {
            Job job = dto.JobType switch
            {
                0
                    => new JobSurveillance(
                        dto.Email,
                        new JobLocation(
                            new Coordinates(
                                dto.Location.StartingPoint.BuildingCode,
                                dto.Location.StartingPoint.FloorNumber,
                                dto.Location.StartingPoint.X,
                                dto.Location.StartingPoint.Y
                            ),
                            new Coordinates(
                                dto.Location.EndingPoint.BuildingCode,
                                dto.Location.EndingPoint.FloorNumber,
                                dto.Location.EndingPoint.X,
                                dto.Location.EndingPoint.Y
                            )
                        ),
                        new JobContact(
                            dto.Surveillance.Contact.Name,
                            dto.Surveillance.Contact.Phone
                        )
                    ),

                1
                    => new JobDelivery(
                        dto.Email,
                        new JobLocation(
                            new Coordinates(
                                dto.Location.StartingPoint.BuildingCode,
                                dto.Location.StartingPoint.FloorNumber,
                                dto.Location.StartingPoint.X,
                                dto.Location.StartingPoint.Y
                            ),
                            new Coordinates(
                                dto.Location.EndingPoint.BuildingCode,
                                dto.Location.EndingPoint.FloorNumber,
                                dto.Location.EndingPoint.X,
                                dto.Location.EndingPoint.Y
                            )
                        ),
                        new JobContact(
                            dto.Delivery.PickupContact.Name,
                            dto.Delivery.PickupContact.Phone
                        ),
                        new JobContact(
                            dto.Delivery.DeliveryContact.Name,
                            dto.Delivery.DeliveryContact.Phone
                        ),
                        new JobConfirmationCode(dto.Delivery.ConfirmationCode),
                        dto.Delivery.Description
                    ),

                _ => throw new ArgumentException("Invalid job type")
            };

            await this._repo.AddAsync(job);
            await this._unitOfWork.CommitAsync();

            return dto;
        }

        public async Task<List<Job>> GetByFilter(FilterDTO dto)
        {
            var strategy = JobFilterContext.FilterStrategy(dto);
            var jobs = await _repo.Filter(strategy);
            _ = await _unitOfWork.CommitAsync(); // ??

            return jobs;
        }

        public async Task<List<Job>> GetByStatus(string state)
        {
            var jobs = await _repo.GetByState(JobState.FromString(state));
            _ = await _unitOfWork.CommitAsync(); // ??

            return jobs;
        }

        public async Task<Job> UpdateJob(UpdatingJobDto dto)
        {
            var job = await _repo.GetByIdAsync(new JobId(dto.JobId));

            if (job == null)
            {
                throw new NotFoundException($"No job found: {dto.JobId}");
            }

            var props = new JobUpdateProps { Status = JobState.FromString(dto.JobStatus) };

            _ = job.Update(props);
            var updatedJob = await _repo.Update(job);
            _ = await _unitOfWork.CommitAsync();

            // TODO: DTO
            return updatedJob;
        }

        public async Task<PlannedTasksDTO> JobSequence(RobotTasksDTO dto)
        {
            var keypairs = new List<KeyValuePair<string, TaskSequenceDto>>();
            foreach ((var key, var tasks) in dto.RobotTasks)
            {
                var jobs = new List<Job>();
                foreach (var t in tasks)
                {
                    var job = await _repo.GetByIdAsync(new JobId(t.id));
                    if (job.Status != JobStateEnum.APPROVED)
                        throw new BusinessRuleValidationException(
                            $"Job {job.Id.Value} is not currently with the state of Approved"
                        );

                    jobs.Add(job);
                }

                var sequence = await _planning.ComputeSequence(
                    ComputeSequenceMapper.ToDTO(dto.Algorithm, jobs)
                );

                // foreach (var j in jobs)
                // {
                //     _ = await UpdateJob(
                //         new UpdatingJobDto { JobId = j.Id.Value, JobStatus = "Planned" }
                //     );
                // }

                var jobSequence = new Sequence(
                    jobs,
                    sequence.cost,
                    key,
                    new Coordinates(
                        sequence.initialPosition.building,
                        sequence.initialPosition.floor,
                        sequence.initialPosition.x,
                        sequence.initialPosition.y
                    )
                );

                await this._sequenceRepo.AddAsync(jobSequence);
                await this._unitOfWork.CommitAsync();

                keypairs.Add(new KeyValuePair<string, TaskSequenceDto>(key, sequence));
            }

            var result = keypairs.ToDictionary(x => x.Key, x => x.Value);
            return new PlannedTasksDTO { RobotTasks = result };
        }
    }
}
