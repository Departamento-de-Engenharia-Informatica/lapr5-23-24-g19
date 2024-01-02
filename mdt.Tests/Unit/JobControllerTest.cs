using DDDNetCore.Infraestructure.Jobs;
using DDDSample1.Controllers;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Jobs.DTO;
using DDDSample1.Domain.Sequences;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;
using Microsoft.AspNetCore.Mvc;
using Moq;

namespace mdt.Tests.Unit
{
    [TestFixture, Category("Unit"), Category("Controller")]
    public class JobControllerTest
    {
        private Mock<IJobRepository> _repo;
        private Mock<IPlanningAdapter> _planning;
        private Mock<ISequenceRepository> _sequence;
        private Mock<IUnitOfWork> _unitOfWork;
        private Mock<JobService> _service;
        private JobsController _controller;

        [SetUp]
        public void Setup()
        {
            _repo = new Mock<IJobRepository>();
            _planning = new Mock<IPlanningAdapter>();
            _sequence = new Mock<ISequenceRepository>();
            _unitOfWork = new Mock<IUnitOfWork>();
            _service = new Mock<JobService>(
                _unitOfWork.Object,
                _repo.Object,
                _planning.Object,
                _sequence.Object
            );
            _controller = new JobsController(_service.Object);
        }

        [Test]
        public async Task TestCreateJobShouldCreateJob()
        {
            var dto = new CreatingJobDto(
                "marco@isep.ipp.pt",
                new JobLocationDto(
                    new CoordinatesDto("B", 2, 1, 2),
                    new CoordinatesDto("B", 2, 1, 2)
                ),
                0,
                null,
                new JobSurveillanceDto(new JobContactDto("Marco", 997456123))
            );

            _service.Setup(s => s.AddAsync(dto)).Returns(Task.FromResult(dto));

            var result = await _controller.Create(dto);

            _service.Verify(s => s.AddAsync(dto), Times.Once);
            Assert.That(result.Result, Is.InstanceOf<OkObjectResult>());
            var okResult = result.Result as OkObjectResult;
            Assert.That(okResult.Value, Is.EqualTo(dto));
        }

        [Test]
        public async Task TestGetByStatusShouldReturnJobs()
        {
            string targetStatus = "PENDING";
            var expectedJobs = new List<Job>
            {
                new JobSurveillance(
                    "marco@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact("Marco Maia", 992123123)
                ),
                new JobSurveillance(
                    "andre@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact("Andre Rocha", 997771123)
                ),
            };

            _service.Setup(s => s.GetByStatus(targetStatus)).Returns(Task.FromResult(expectedJobs));
            var result = await _controller.GetByStatus(targetStatus);

            _service.Verify(s => s.GetByStatus(targetStatus), Times.Once);
            Assert.That(result.Result, Is.InstanceOf<OkObjectResult>());
            var okResult = result.Result as OkObjectResult;
            Assert.That(okResult.Value, Is.EqualTo(expectedJobs));
        }

        [Test]
        public async Task JobSequenceShouldReturnSequence()
        {
            var dto = new RobotTasksDTO();
            var expectedSequence = new List<PlannedRobotTasksDTO>();

            _service.Setup(x => x.JobSequence(dto)).ReturnsAsync(expectedSequence);

            var result = await _controller.JobSequence(dto);

            _service.Verify(x => x.JobSequence(dto), Times.Once);
            Assert.That(result.Result, Is.InstanceOf<OkObjectResult>());
            var okResult = result.Result as OkObjectResult;
            Assert.That(okResult.Value, Is.EqualTo(expectedSequence));
        }

        [Test]
        public async Task UpdateJobFailsIfJobNotFound()
        {
            var dto = new UpdatingJobDto
            {
                JobId = "01268906-2204-459d-8e86-335839f4e413",
                JobStatus = "Approved"
            };

            var errMsg = "Job not found";
            _ = _service
                .Setup(svc => svc.UpdateJob(dto))
                .Throws(() => new NotFoundException(errMsg));

            var result = await _controller.Update(dto.JobId, dto);
            Assert.That(result.Result, Is.InstanceOf<NotFoundObjectResult>());

            var notFoundResult = result.Result as NotFoundObjectResult;
            Assert.That(notFoundResult?.Value, Is.EqualTo(errMsg));
        }

        public JobControllerTest()
        {
            _repo = new Mock<IJobRepository>();
            _planning = new Mock<IPlanningAdapter>();
            _sequence = new Mock<ISequenceRepository>();
            _unitOfWork = new Mock<IUnitOfWork>();
            _service = new Mock<JobService>(
                _unitOfWork.Object,
                _repo.Object,
                _planning.Object,
                _sequence.Object
            );
            _controller = new JobsController(_service.Object);
        }
    }
}
