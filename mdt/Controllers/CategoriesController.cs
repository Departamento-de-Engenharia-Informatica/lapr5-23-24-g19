using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Categories;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class CategoriesController : ControllerBase
    {
        private readonly CategoryService _service;

        public CategoriesController(CategoryService service)
        {
            _service = service;
        }

        // GET: api/Categories
        [HttpGet]
        public async Task<ActionResult<IEnumerable<CategoryDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Categories/5
        [HttpGet("{id}")]
        public async Task<ActionResult<CategoryDto>> GetGetById(Guid id)
        {
            var cat = await _service.GetByIdAsync(new CategoryId(id));

            if (cat == null)
            {
                return NotFound();
            }

            return cat;
        }

        // POST: api/Categories
        [HttpPost]
        public async Task<ActionResult<CategoryDto>> Create(CreatingCategoryDto dto)
        {
            var cat = await _service.AddAsync(dto);

            return CreatedAtAction(nameof(GetGetById), new { id = cat.Id }, cat);
        }

        
        // PUT: api/Categories/5
        [HttpPut("{id}")]
        public async Task<ActionResult<CategoryDto>> Update(Guid id, CategoryDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var cat = await _service.UpdateAsync(dto);
                
                if (cat == null)
                {
                    return NotFound();
                }
                return Ok(cat);
            }
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }

        // Inactivate: api/Categories/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<CategoryDto>> SoftDelete(Guid id)
        {
            var cat = await _service.InactivateAsync(new CategoryId(id));

            if (cat == null)
            {
                return NotFound();
            }

            return Ok(cat);
        }
        
        // DELETE: api/Categories/5
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<CategoryDto>> HardDelete(Guid id)
        {
            try
            {
                var cat = await _service.DeleteAsync(new CategoryId(id));

                if (cat == null)
                {
                    return NotFound();
                }

                return Ok(cat);
            }
            catch(BusinessRuleValidationException ex)
            {
               return BadRequest(new {Message = ex.Message});
            }
        }
    }
}