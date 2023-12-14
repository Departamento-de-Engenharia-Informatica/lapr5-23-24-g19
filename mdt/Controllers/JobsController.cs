using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Jobs;


namespace DDDSample1.Controllers
{
    [Route("api/Jobs")]
    [ApiController]
    public class JobsController : ControllerBase
    {
        private readonly JobService _service;

        public JobsController(JobService service)
        {
            _service = service;
        }

        // POST: api/Jobs
        [HttpPost]
        public async Task<ActionResult<CreatingJobDto>> Create(CreatingJobDto dto)
        {
            var job = await _service.AddAsync(dto);

            return Ok(job);
        }

        // PATCH: api/Jobs/{id}
        [HttpPatch("{id}")]
        public async Task<ActionResult<JobDto>> Update(string id, UpdatingJobDto dto)
        {
            throw new System.NotImplementedException();
        }

        // GET: api/Jobs/non-approved
        [HttpGet("non-approved")]
        public async Task<ActionResult<JobDto[]>> GetNonApproved()
        {
            throw new System.NotImplementedException();
        }

        // GET: api/Jobs/{filter}
        // filter can be: "state", "device" or "client"
        [HttpGet("{filter}")]
        public async Task<ActionResult<JobDto[]>> GetByFilter(string filter)
        {
            throw new System.NotImplementedException();
        }

        // GET: api/Jobs/approved-jobs-sequence
        [HttpGet("approved-jobs-sequence")]
        public async Task<ActionResult<JobDto[]>> GetApprovedJobsSequence()
        {
            throw new System.NotImplementedException();
        }
    }
}