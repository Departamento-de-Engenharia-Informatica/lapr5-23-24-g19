using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Jobs.DTO;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{
    [Route("api/jobs")]
    [ApiController]
    public class JobsController : ControllerBase
    {
        private readonly JobService _service;
        private readonly SequenceService _serviceSeq;

        public JobsController(JobService service, SequenceService seqSrv)
        {
            _service = service;
            _serviceSeq = seqSrv;
        }

        [HttpGet("status")]
        public async Task<ActionResult<string>> Status()
        {
            return Ok("Status ok");
        }

        // POST: api/Jobs
        [HttpPost]
        public async Task<ActionResult<CreatingJobDto>> Create(CreatingJobDto dto)
        {
            // var options = new JsonSerializerOptions
            // {
            //     WriteIndented = true // This sets the indentation
            // };
            // Console.WriteLine("\nhelo\n");
            // Console.WriteLine(JsonSerializer.Serialize(dto, options));
            // Console.WriteLine("\nbye\n");
            var job = await _service.AddAsync(dto);

            return Ok(job);
        }

        // GET: api/jobs/{id}
        [HttpGet("{id}")]
        public async Task<ActionResult<string>> GetGetById(string id)
        {
            var job = await _service.GetByIdAsync(id);

            if (job == null)
            {
                return NotFound();
            }

            return Ok(job);
        }

        // PATCH: api/jobs/{id}
        [HttpPatch("{id}")]
        public async Task<ActionResult<JobDto>> Update(string id, UpdatingJobDto dto)
        {
            if (dto == null)
            {
                return BadRequest();
            }

            dto.JobId = id;

            try
            {
                var updatedJob = await _service.UpdateJob(dto);
                return Ok(updatedJob);
            }
            catch (NotFoundException ex)
            {
                return NotFound(ex.Message);
            }
            catch (Exception ex) when (ex is ArgumentException or BusinessRuleValidationException)
            {
                return BadRequest(ex.Message);
            }
        }

        // GET: api/jobs/non-approved
        [HttpGet("non-approved")]
        public async Task<ActionResult<JobDto[]>> GetNonApproved()
        {
            throw new System.NotImplementedException();
        }

        // GET: api/jobs/filter/{filter}?rule=someRule
        [HttpGet("filter")]
        // GET: api/jobs?filter=filter&rule=someRule
        // [HttpGet], com parametros para filter e rule
        public async Task<ActionResult<Job>> GetByFilter(string filter, string rule)
        {

            Console.WriteLine(filter);
            Console.WriteLine(rule);
            Console.WriteLine("ssss");

            var dto = FilterMapper.ToDto(filter, rule);

            try
            {
                var jobs = await _service.GetByFilter(dto);
                if (jobs == null)
                {
                    return NotFound("No tasks found");
                }
                return Ok(jobs);
            }
            catch (NotFoundException ex)
            {
                return NotFound(ex.Message);
            }
            catch (Exception ex) when (ex is ArgumentException or BusinessRuleValidationException)
            {
                return BadRequest(ex.Message);
            }
        }

        [HttpGet("")]
        public async Task<ActionResult<JobDto[]>> GetByStatus([FromQuery] string status)
        {
            var jobs = await _service.GetByStatus(status);
            if (jobs == null)
            {
                return NotFound("Tasks not found");
            }

            return Ok(jobs);
        }

        // GET: api/jobs/approved-jobs-sequence
        [HttpGet("approved-jobs-sequence")]
        public async Task<ActionResult<JobDto[]>> GetApprovedJobsSequence()
        {
            throw new System.NotImplementedException();
        }

        [HttpGet("sequence/algorithms")]
        public async Task<ActionResult<string[]>> GetJobSequenceAlgorithms()
        {
            try
            {
                var algorithms = await _service.JobSequenceAlgorithms();
                return Ok(algorithms);
            }
            catch (Exception e)
            {
                return new ObjectResult(e.Message)
                {
                    StatusCode = 502,
                };
            }
        }


        [HttpPatch("sequence")]
        public async Task<ActionResult<List<PlannedRobotTasksDTO>>> JobSequence([FromBody] RobotTasksDTO dto)
        {
            try
            {
                var sequence = await _service.JobSequence(dto);
                return Ok(sequence);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(ex.Message);
            }
        }

         [HttpGet("robots/{name}")]
        public async Task<ActionResult<List<string>>> RobotSequence(string name)
        {
            Console.WriteLine("hello");
            try
            {
                var sequence = await _serviceSeq.RobotSequence(name);
                return Ok(sequence);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(ex.Message);
            }
        }
    }
}
