using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Families;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class FamiliesController : ControllerBase
    {
        private readonly FamilyService _service;

        public FamiliesController(FamilyService service)
        {
            _service = service;
        }

        // GET: api/Families
        [HttpGet]
        public async Task<ActionResult<IEnumerable<FamilyDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Families/F1
        [HttpGet("{id}")]
        public async Task<ActionResult<FamilyDto>> GetGetById(String id)
        {
            var fam = await _service.GetByIdAsync(new FamilyId(id));

            if (fam == null)
            {
                return NotFound();
            }

            return fam;
        }

        // POST: api/Families
        [HttpPost]
        public async Task<ActionResult<FamilyDto>> Create(FamilyDto dto)
        {
            var fam = await _service.AddAsync(dto);

            return CreatedAtAction(nameof(GetGetById), new { id = fam.Id }, fam);
        }

        
        // PUT: api/Families/F5
        [HttpPut("{id}")]
        public async Task<ActionResult<FamilyDto>> Update(String id, FamilyDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var fam = await _service.UpdateAsync(dto);
                
                if (fam == null)
                {
                    return NotFound();
                }
                return Ok(fam);
            }
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }

        // Inactivate: api/Families/F5
        [HttpDelete("{id}")]
        public async Task<ActionResult<FamilyDto>> SoftDelete(String id)
        {
            var fam = await _service.InactivateAsync(new FamilyId(id));

            if (fam == null)
            {
                return NotFound();
            }

            return Ok(fam);
        }
        
        // DELETE: api/Families/F5
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<FamilyDto>> HardDelete(String id)
        {
            try
            {
                var fam = await _service.DeleteAsync(new FamilyId(id));

                if (fam == null)
                {
                    return NotFound();
                }

                return Ok(fam);
            }
            catch(BusinessRuleValidationException ex)
            {
               return BadRequest(new {Message = ex.Message});
            }
        }
    }
}