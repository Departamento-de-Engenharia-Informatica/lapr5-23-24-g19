using DDDNetCore.Infraestructure.Jobs;
using DDDSample1.Controllers;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Jobs.DTO;
using DDDSample1.Domain.Sequences;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;
using Microsoft.AspNetCore.Mvc;
using Moq;

namespace mdt.Tests.Integration
{
    [TestFixture, Category("Integration"), Category("Service")]
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
            var okObjectResult = result.Result as OkObjectResult;
            Assert.That(okObjectResult.Value, Is.EqualTo(dto));
        }

        [Test]
        [TestCase("PENDING")]
        [TestCase("APPROVED")]
        [TestCase("REJECTED")]
        public async Task TestGetByStatusShouldReturnJobs(string targetStatus)
        {
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
            Assert.IsInstanceOf<OkObjectResult>(result.Result);
            var okObjectResult = result.Result as OkObjectResult;
            Assert.That(okObjectResult.Value, Is.EqualTo(expectedJobs));

            _repo.Verify(
                r => r.GetByState(It.Is<JobStateEnum>(s => s.ToString() == targetStatus)),
                Times.Once
            );
        }

        [Test]
        public async Task TestJobSequenceShouldBeAbleToSequenceTasks()
        {
            var jobs = new List<Job>
            {
                new JobDelivery(
                    "test1@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact(),
                    new JobContact(),
                    new JobConfirmationCode(42069),
                    "Test description"
                ),
                new JobDelivery(
                    "test2@isep.ipp.pt",
                    new JobLocation(new Coordinates("C", 2, 2, 3), new Coordinates("D", 2, 10, 14)),
                    new JobContact(),
                    new JobContact(),
                    new JobConfirmationCode(43311),
                    "Test description"
                ),
                new JobSurveillance(
                    "test3@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact()
                ),
                new JobSurveillance(
                    "test4@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact()
                )
            };

            foreach (var j in jobs)
            {
                j.Update(new JobUpdateProps { Status = JobStateEnum.APPROVED });
            }

            var dto = new RobotTasksDTO
            {
                RobotTasks = new Dictionary<string, List<TaskIdsDTO>>
                {
                    {
                        "Robot1",
                        new List<TaskIdsDTO>
                        {
                            new TaskIdsDTO { id = jobs.ElementAt(0).Id.Value, type = "DELIVERY" },
                            new TaskIdsDTO { id = jobs.ElementAt(1).Id.Value, type = "DELIVERY" },
                            new TaskIdsDTO
                            {
                                id = jobs.ElementAt(2).Id.Value,
                                type = "SURVEILLANCE"
                            },
                            new TaskIdsDTO
                            {
                                id = jobs.ElementAt(3).Id.Value,
                                type = "SURVEILLANCE"
                            }
                        }
                    }
                },
                Algorithm = "Genetic"
            };

            var taskSequenceDto = new TaskSequenceDto
            {
                cost = 42,
                initialPosition = new Position
                {
                    building = "B",
                    floor = 1,
                    x = 2,
                    y = 3
                },
                order = new List<TaskUnit>
                {
                    new TaskUnit
                    {
                        start = new Position
                        {
                            building = "B",
                            floor = 1,
                            x = 2,
                            y = 3
                        },
                        end = new Position
                        {
                            building = "B",
                            floor = 1,
                            x = 2,
                            y = 3
                        },
                        taskId = jobs.ElementAt(0).Id.Value
                    },
                    new TaskUnit
                    {
                        start = new Position
                        {
                            building = "C",
                            floor = 2,
                            x = 2,
                            y = 3
                        },
                        end = new Position
                        {
                            building = "D",
                            floor = 2,
                            x = 10,
                            y = 14
                        },
                        taskId = jobs.ElementAt(1).Id.Value
                    },
                    new TaskUnit
                    {
                        start = new Position
                        {
                            building = "B",
                            floor = 1,
                            x = 2,
                            y = 3
                        },
                        end = new Position
                        {
                            building = "B",
                            floor = 1,
                            x = 2,
                            y = 3
                        },
                        taskId = jobs.ElementAt(2).Id.Value
                    },
                    new TaskUnit
                    {
                        start = new Position
                        {
                            building = "B",
                            floor = 1,
                            x = 2,
                            y = 3
                        },
                        end = new Position
                        {
                            building = "B",
                            floor = 1,
                            x = 2,
                            y = 3
                        },
                        taskId = jobs.ElementAt(3).Id.Value
                    }
                }
            };

            var expected = new List<PlannedRobotTasksDTO>
            {
                new PlannedRobotTasksDTO { RobotName = "Robot1", Tasks = taskSequenceDto }
            };

            _planning
                .Setup(planning => planning.ComputeSequence(It.IsAny<ComputeSequenceDto>()))
                .ReturnsAsync(taskSequenceDto);

            _repo
                .Setup(repo => repo.GetByIdAsync(It.IsAny<JobId>()))
                .ReturnsAsync((JobId jobId) => jobs.FirstOrDefault(j => j.Id == jobId));

            _repo.Setup(repo => repo.Update(It.IsAny<Job>())).ReturnsAsync((Job job) => job);

            _sequence.Setup(repo => repo.AddAsync(It.IsAny<Sequence>())).Verifiable();

            var result = await _controller.JobSequence(dto);

            _repo.Verify(
                repo => repo.GetByIdAsync(It.IsAny<JobId>()),
                Times.Exactly(dto.RobotTasks["Robot1"].Count * 2)
            );

            _planning.Verify(
                planning => planning.ComputeSequence(It.IsAny<ComputeSequenceDto>()),
                Times.Once
            );

            _unitOfWork.Verify(
                unitOfWork => unitOfWork.CommitAsync(),
                Times.Exactly(dto.RobotTasks["Robot1"].Count + 1)
            );

            Assert.IsNotNull(result);
            Assert.IsInstanceOf<OkObjectResult>(result.Result);
            var okObjectResult = result.Result as OkObjectResult;
            Assert.That(okObjectResult.Value, Is.InstanceOf<List<PlannedRobotTasksDTO>>());
        }

        [Test]
        public async Task UpdateJobSucceedsWithRightParameters()
        {
            var dto = new UpdatingJobDto
            {
                JobId = "01268906-2204-459d-8e86-335839f4e413",
                JobStatus = "Approved"
            };

            var job = new JobDelivery(
                                                         "test1@isep.ipp.pt",
                                                         new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                                                         new JobContact(),
                                                         new JobContact(),
                                                         new JobConfirmationCode(42069),
                                                         "Test description"
                                                        );

            _ = _repo
                .Setup(repo => repo.GetByIdAsync(It.IsAny<JobId>()))
                .ReturnsAsync(job);
            _ = _repo
                .Setup(repo => repo.Update(It.IsAny<Job>()))
                .ReturnsAsync(job);


            var result = await _controller.Update(dto.JobId, dto);

            _repo.Verify(repo => repo.GetByIdAsync(It.Is<JobId>(id => id.Value == dto.JobId)), Times.Once);
            _repo.Verify(repo => repo.Update(It.Is<Job>(j => j.Email == job.Email)), Times.Once);
            _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);

            Assert.That(result, Is.Not.Null);
            Assert.That(result.Result, Is.InstanceOf<OkObjectResult>());
            var okObjectResult = result.Result as OkObjectResult;
            Assert.That(okObjectResult?.Value, Is.InstanceOf<Job>());

            var value = okObjectResult!.Value as Job;
            Assert.Multiple(() =>
            {
                Assert.That(value?.Email, Is.EqualTo(job.Email));
                Assert.That(value?.Status, Is.EqualTo(JobStateEnum.APPROVED));
            });
        }

        [Test]
        public async Task UpdateJobFailsIfBusinessRulesAreBroken()
        {
            var dto = new UpdatingJobDto
            {
                JobId = "01268906-2204-459d-8e86-335839f4e413",
                JobStatus = "Approved"
            };
            var job = new JobDelivery(
                                                         "test1@isep.ipp.pt",
                                                         new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                                                         new JobContact(),
                                                         new JobContact(),
                                                         new JobConfirmationCode(42069),
                                                         "Test description"
                                                        ).Update(new JobUpdateProps { Status = JobStateEnum.REJECTED });


            _ = _repo
                .Setup(repo => repo.GetByIdAsync(It.IsAny<JobId>()))
                .ReturnsAsync(job);
            _ = _repo
                .Setup(repo => repo.Update(It.IsAny<Job>()))
                .ReturnsAsync(job);

            var result = await _controller.Update(dto.JobId, dto);

            Assert.That(result, Is.Not.Null);
            Assert.That(result.Result, Is.InstanceOf<BadRequestObjectResult>());
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
