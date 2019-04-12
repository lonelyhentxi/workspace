using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Eru.Server.Data.Models;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Eru.Server.Services;
using Microsoft.AspNetCore.Mvc;

namespace Eru.Server.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class ApplicationsController : ControllerBase
    {
        private readonly ApplicationService _applicationService;

        public ApplicationsController(ApplicationService applicationService)
        {
            _applicationService = applicationService;
        }

        // GET: api/applications
        [HttpGet]
        [ProducesResponseType(200)]
        public async Task<ActionResult<ResultOutDto<IEnumerable<Application>>>> GetApplications(
            [FromQuery] ApplicationFilterInDto filterOptions)
        {
            return Ok(ResultOutDtoBuilder
                .Success(await _applicationService.Filter(filterOptions)));
        }

        // GET: api/applications/5
        [HttpGet("{id}")]
        [ProducesResponseType(200)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<Application>>> GetApplication(string id)
        {
            if (!Guid.TryParse(id, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<Application>(new FormatException(), "Error guid format."));
            }

            try
            {
                var application = await _applicationService.Get(guid, true);
                return Ok(ResultOutDtoBuilder.Success(application));
            }
            catch (NotExistedException e)
            {
                return NotFound(
                    ResultOutDtoBuilder.Fail<Application>(
                        e, "Not exist."));
            }
        }

        // PUT: api/applications/5

        [HttpPut("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        [ProducesResponseType(409)]
        public async Task<ActionResult<ResultOutDto<object>>> PutApplication(string id, Application application)
        {
            if (!Guid.TryParse(id, out Guid guid) || guid != application.Id)
            {
                return BadRequest(ResultOutDtoBuilder.Fail<User>(new FormatException(), "Error id format"));
            }

            try
            {
                await _applicationService.Update(application);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(ResultOutDtoBuilder.Fail<Application>(e, "Not exist."));
            }
            catch (ExistedConflictException e)
            {
                return Conflict(
                    ResultOutDtoBuilder.Fail<Application>(e, "New name conflict with other existed application."));
            }
        }

        // POST: api/Applications
        [HttpPost]
        [ProducesResponseType(200)]
        [ProducesResponseType(409)]
        public async Task<ActionResult<ResultOutDto<Application>>> PostApplication(ApplicationCreateInDto createOptions)
        {
            try
            {
                var applications = await _applicationService.Create(createOptions);
                return Ok(ResultOutDtoBuilder.Success(applications));
            }
            catch (ExistedConflictException e)
            {
                return Conflict(
                    ResultOutDtoBuilder.Fail<Application>(e, "New name conflict with other existed application."));
            }
        }

        // DELETE: api/Applications/5
        [HttpDelete("{id}")]
        [ProducesResponseType(204)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        public async Task<ActionResult<ResultOutDto<object>>> DeleteApplication(string id)
        {
            if (!Guid.TryParse(id, out Guid guid))
            {
                return BadRequest(ResultOutDtoBuilder
                    .Fail<Application>(new FormatException(), "Error guid format."));
            }

            try
            {
                await _applicationService.Remove(guid);
                return NoContent();
            }
            catch (NotExistedException e)
            {
                return NotFound(
                    ResultOutDtoBuilder.Fail<Application>(
                        e, "Not exist."));
            }
        }
    }
}