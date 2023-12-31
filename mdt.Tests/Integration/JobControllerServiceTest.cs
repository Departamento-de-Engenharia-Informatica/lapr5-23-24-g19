using DDDNetCore.Infraestructure.Jobs;
using DDDSample1.Controllers;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Sequences;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;
using Microsoft.AspNetCore.Mvc;
using Moq;

namespace mdt.Tests.Integration
{
    [TestFixture, Category("Unit"), Category("Service")]
    public class JobControllerServiceTest
    {
        private Mock<IJobRepository> _repo;
        private Mock<IPlanningAdapter> _planning;
        private Mock<ISequenceRepository> _sequence;
        private Mock<IUnitOfWork> _unitOfWork;
        private JobService _service;
        private JobsController _controller;

        [SetUp]
        public void Setup()
        {
            _repo = new Mock<IJobRepository>();
            _planning = new Mock<IPlanningAdapter>();
            _sequence = new Mock<ISequenceRepository>();
            _unitOfWork = new Mock<IUnitOfWork>();
            _service = new JobService(
                _unitOfWork.Object,
                _repo.Object,
                _planning.Object,
                _sequence.Object
            );
            _controller = new JobsController(_service);
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

            _repo.Setup(r => r.AddAsync(It.IsAny<Job>())).Verifiable();
            _unitOfWork.Setup(u => u.CommitAsync()).Verifiable();

            var result = await _controller.Create(dto);

            _repo.Verify(r => r.AddAsync(It.IsAny<Job>()), Times.Once);
            _unitOfWork.Verify(u => u.CommitAsync(), Times.Once);

            Assert.That(result.Result, Is.InstanceOf<OkObjectResult>());
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

            _repo.Setup(r => r.GetByState(It.IsAny<JobStateEnum>())).ReturnsAsync(expectedJobs);

            var result = await _controller.GetByStatus(targetStatus);

            Assert.IsNotNull(result);
            Assert.That(result.Result, Is.InstanceOf<OkObjectResult>());

            _repo.Verify(
                r => r.GetByState(It.Is<JobStateEnum>(s => s.ToString() == targetStatus)),
                Times.Once
            );
        }

        public JobControllerServiceTest()
        {
            _repo = new Mock<IJobRepository>();
            _planning = new Mock<IPlanningAdapter>();
            _sequence = new Mock<ISequenceRepository>();
            _unitOfWork = new Mock<IUnitOfWork>();
            _service = new JobService(
                _unitOfWork.Object,
                _repo.Object,
                _planning.Object,
                _sequence.Object
            );
            _controller = new JobsController(_service);
        }
    }
}
