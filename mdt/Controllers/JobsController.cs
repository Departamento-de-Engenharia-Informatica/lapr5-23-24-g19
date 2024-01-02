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

        public JobsController(JobService service)
        {
            _service = service;
        }

        [HttpGet("status")]
        public IActionResult Status()
        {
            return Ok();
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

        // GET: api/jobs/filter?filter={:filter}&rule={:rule}
        [HttpGet("filter")]
        public async Task<ActionResult<List<JobDto>>> GetByFilter(string filter, string rule)
        {
            try
            {
                var dto = FilterMapper.ToDto(filter, rule);
                var jobs = await _service.GetByFilter(dto);
                return Ok(jobs);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
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
                return new ObjectResult(e.Message) { StatusCode = 502, };
            }
        }

        [HttpPatch("sequence")]
        public async Task<ActionResult<List<PlannedRobotTasksDTO>>> JobSequence(
            [FromBody] RobotTasksDTO dto
        )
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
    }
}
