using DDDNetCore.Infraestructure.Jobs;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Sequences;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;
using Moq;

namespace mdt.Tests
{
    public class Tests
    {
        private Mock<IJobRepository> _repo;
        private Mock<IPlanningAdapter> _planning;
        private Mock<ISequenceRepository> _sequence;
        private Mock<IUnitOfWork> _unitOfWork;
        private JobService _service;

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
        }

        [Test]
        public async Task TestGetByIdAsyncReturnsSerializedJobWhenExists()
        {
            var expectedJob = new JobSurveillance(
                "marco@isep.ipp.pt",
                new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                new JobContact("Marco Maia", 992123123)
            );
            string serializedJob =
                "{\n"
                + "  \"Email\": \"marco@isep.ipp.pt\",\n"
                + "  \"Location\": {\n"
                + "    \"StartingPoint\": {\n"
                + "      \"BuildingCode\": \"B\",\n"
                + "      \"FloorNumber\": 1,\n"
                + "      \"X\": 2,\n"
                + "      \"Y\": 3\n"
                + "    },\n"
                + "    \"EndingPoint\": {\n"
                + "      \"BuildingCode\": \"B\",\n"
                + "      \"FloorNumber\": 1,\n"
                + "      \"X\": 2,\n"
                + "      \"Y\": 3\n"
                + "    }\n"
                + "  },\n"
                + "  \"Status\": 0,\n"
                + "  \"JobType\": 0,\n"
                + "  \"Id\": {\n"
                + "    \"Value\": \""
                + expectedJob.Id.Value
                + "\"\n"
                + "  }\n"
                + "}";

            _repo
                .Setup(r => r.GetByIdAsync(new JobId(expectedJob.Id.Value)))
                .ReturnsAsync(expectedJob);

            var result = await _service.GetByIdAsync(expectedJob.Id.Value);

            Assert.That(result, Is.EqualTo(serializedJob));
        }

        [Test]
        public async Task TestGetByIdAsyncReturnsNullWhenJobDoesNotExist()
        {
            string jobId = new Guid().ToString();

            _repo.Setup(r => r.GetByIdAsync(new JobId(jobId))).ReturnsAsync((Job)null!);

            var result = await _service.GetByIdAsync(jobId);

            Assert.IsNull(result);
        }

        [Test]
        public async Task TestAddAsyncShouldAddJobAndCommit()
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

            var result = await _service.AddAsync(dto);

            _repo.Verify(r => r.AddAsync(It.IsAny<Job>()), Times.Once);
            _unitOfWork.Verify(u => u.CommitAsync(), Times.Once);

            Assert.That(result, Is.EqualTo(dto));
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

            var result = await _service.GetByStatus(targetStatus);

            Assert.IsNotNull(result);
            Assert.That(result.Count, Is.EqualTo(expectedJobs.Count));
            Assert.IsTrue(expectedJobs.All(job => result.Contains(job)));

            // verify that GetByState method was called with the correct JobState
            _repo.Verify(
                r => r.GetByState(It.Is<JobStateEnum>(s => s.ToString() == targetStatus)),
                Times.Once
            );
        }
    }
}
