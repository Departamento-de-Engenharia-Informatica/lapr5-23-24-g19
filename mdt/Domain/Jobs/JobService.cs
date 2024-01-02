using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Threading.Tasks;
using DDDNetCore.Infraestructure.Jobs;
using DDDSample1.Domain.Jobs.DTO;
using DDDSample1.Domain.Jobs.Filter;
using DDDSample1.Domain.Jobs.Mapper;
using DDDSample1.Domain.Sequences;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;

namespace DDDSample1.Domain.Jobs
{
    public class JobService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IJobRepository _repo;
        private readonly IPlanningAdapter _planning;
        private readonly ISequenceRepository _sequenceRepo;

        public JobService(
            IUnitOfWork unitOfWork,
            IJobRepository repo,
            IPlanningAdapter planning,
            ISequenceRepository sequenceRepo
        )
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _planning = planning;
            _sequenceRepo = sequenceRepo;
        }

        public async Task<string> GetByIdAsync(string id)
        {
            var job = await _repo.GetByIdAsync(new JobId(id));

            if (job == null)
            {
                return null;
            }

            var options = new JsonSerializerOptions
            {
                WriteIndented = true // This sets the indentation
            };

            return JsonSerializer.Serialize(job, options);
        }

        public virtual async Task<CreatingJobDto> AddAsync(CreatingJobDto dto)
        {
            await _repo.AddAsync(JobMapper.ToDomain(dto));
            await _unitOfWork.CommitAsync();

            return dto;
        }

        public async Task<List<Job>> GetByFilter(FilterDTO dto)
        {
            var strategy = JobFilterFactory.GetFilterStrategy(dto);
            var jobs = await _repo.Filter(strategy);
            _ = await _unitOfWork.CommitAsync(); // ??

            return jobs;
        }

        public virtual async Task<List<Job>> GetByStatus(string state)
        {
            var jobs = await _repo.GetByState(JobState.FromString(state));
            _ = await _unitOfWork.CommitAsync(); // ??

            return jobs;
        }

        public virtual async Task<Job> UpdateJob(UpdatingJobDto dto)
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

        public virtual async Task<List<PlannedRobotTasksDTO>> JobSequence(RobotTasksDTO dto)
        {
            var robotTasks = new List<PlannedRobotTasksDTO>();

            foreach ((var robotName, var tasks) in dto.RobotTasks)
            {
                var jobs = new List<Job>();
                foreach (var t in tasks)
                {
                    var job = await _repo.GetByIdAsync(new JobId(t.id));
                    if (job.Status != JobStateEnum.APPROVED)
                    {
                        throw new BusinessRuleValidationException(
                            $"Job {job.Id.Value} is not currently with the state of Approved"
                        );
                    }

                    jobs.Add(job);
                }

                var sequence = await _planning.ComputeSequence(
                    ComputeSequenceMapper.ToDTO(dto.Algorithm, jobs)
                );

                foreach (var j in jobs)
                {
                    _ = await UpdateJob(
                        new UpdatingJobDto { JobId = j.Id.Value, JobStatus = "Planned" }
                    );
                    Console.WriteLine($"Updated job requested by {j.Email}");
                }

                var jobSequence = new Sequence(
                    jobs,
                    sequence.cost,
                    robotName,
                    new Coordinates(
                        sequence.initialPosition.building,
                        sequence.initialPosition.floor,
                        sequence.initialPosition.x,
                        sequence.initialPosition.y
                    )
                );

                _ = await _sequenceRepo.AddAsync(jobSequence);
                _ = await _unitOfWork.CommitAsync();

                robotTasks.Add(
                    new PlannedRobotTasksDTO { RobotName = robotName, Tasks = sequence }
                );
            }

            return robotTasks;
        }

        public async Task<string[]> JobSequenceAlgorithms()
        {
            return await _planning.GetSequenceAlgorithms();
        }
    }
}
