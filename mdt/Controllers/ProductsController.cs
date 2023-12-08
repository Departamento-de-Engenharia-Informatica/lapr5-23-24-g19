using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Categories;
using DDDSample1.Domain.Products;


namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class ProductsController : ControllerBase
    {
        private readonly ProductService _service;

        public ProductsController(ProductService service)
        {
            _service = service;
        }

        // GET: api/Products
        [HttpGet]
        public async Task<ActionResult<IEnumerable<ProductDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Products/5
        [HttpGet("{id}")]
        public async Task<ActionResult<ProductDto>> GetGetById(Guid id)
        {
            var prod = await _service.GetByIdAsync(new ProductId(id));

            if (prod == null)
            {
                return NotFound();
            }

            return prod;
        }

        // POST: api/Products
        [HttpPost]
        public async Task<ActionResult<ProductDto>> Create(CreatingProductDto dto)
        {
            try
            {
                var prod = await _service.AddAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = prod.Id }, prod);
            }
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }

        
        // PUT: api/Products/5
        [HttpPut("{id}")]
        public async Task<ActionResult<ProductDto>> Update(Guid id, ProductDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var prod = await _service.UpdateAsync(dto);
                
                if (prod == null)
                {
                    return NotFound();
                }
                return Ok(prod);
            }
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }

        // Inactivate: api/Products/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<ProductDto>> SoftDelete(Guid id)
        {
            var prod = await _service.InactivateAsync(new ProductId(id));

            if (prod == null)
            {
                return NotFound();
            }

            return Ok(prod);
        }
        
        // DELETE: api/Products/5
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<ProductDto>> HardDelete(Guid id)
        {
            try
            {
                var prod = await _service.DeleteAsync(new ProductId(id));

                if (prod == null)
                {
                    return NotFound();
                }

                return Ok(prod);
            }
            catch(BusinessRuleValidationException ex)
            {
               return BadRequest(new {Message = ex.Message});
            }
        }
    }
}