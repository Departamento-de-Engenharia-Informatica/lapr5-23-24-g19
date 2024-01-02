using DDDNetCore.Infraestructure.Jobs;
using DDDSample1.Domain.Jobs;
using DDDSample1.Domain.Jobs.DTO;
using DDDSample1.Domain.Jobs.Filter;
using DDDSample1.Domain.Sequences;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;
using Moq;

namespace mdt.Tests.Unit
{
    [TestFixture, Category("Unit"), Category("Service")]
    public class JobServiceTest
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

        [TestCase("CLIENT", "mzc@isep.ipp.pt", null, null)]
        [TestCase("CLIENT", "ajs@isep.ipp.pt", null, null)]
        [TestCase("CLIENT", "jjp@isep.ipp.pt", null, null)]
        [TestCase("CLIENT", "nmb@isep.ipp.pt", null, null)]
        [TestCase("CLIENT", "nvm@isep.ipp.pt", null, null)]
        [TestCase("CLIENT", "nps@isep.ipp.pt", null, null)]
        [TestCase("CLIENT", "nsp@isep.ipp.pt", null, null)]
        [TestCase("CLIENT", "jpl@isep.ipp.pt", null, null)]
        public async Task GetByFilterWithClientFilterReturnsFilteredJobs(
            string filter,
            string email,
            int? state,
            int? type
        )
        {
            // Arrange
            var filterDto = new FilterDTO(filter, email, state, type);
            var matchingJobs = new List<Job>
            {
                new JobSurveillance(
                    email,
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact("Andre Rocha", 997771123)
                )
            };

            var nonMatchingJobs = new List<Job>
            {
                new JobSurveillance(
                    "_test2_@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact()
                ),
                new JobDelivery(
                    "_test3_@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact(),
                    new JobContact(),
                    new JobConfirmationCode(66633),
                    "Test description"
                )
            };

            _ = _repo
                .Setup(repo => repo.Filter(It.IsAny<IJobFilterStrategy>()))
                .ReturnsAsync(matchingJobs);

            // Act
            var result = await _service.GetByFilter(filterDto);

            // Assert
            Assert.That(result, Is.Not.Null);
            CollectionAssert.AreEqual(matchingJobs, result);

            // result must not have members from the nonMatchingJobs
            var nonMatchingOverlap = result.Intersect(nonMatchingJobs);
            Assert.That(nonMatchingOverlap, Is.Empty);

            // right strategy pattern
            _repo.Verify(repo => repo.Filter(It.IsAny<JobClientFilterStrategy>()), Times.Once);
            // transaction commited
            _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [TestCase("CLIENT", "", null, null)]
        [TestCase("TYPE", null, null, 2)]
        [TestCase("TYPE", null, null, 3)]
        [TestCase("TYPE", null, null, -100)]
        [TestCase("STATE", null, 430, null)]
        [TestCase("STATE", null, -100, null)]
        [TestCase("STATE", null, 10, null)]
        [TestCase("STATE", null, null, null)]
        [TestCase("TYPE", null, null, null)]
        [TestCase("CLIENT", null, null, null)]
        public async Task GetByFilterReturnsNoJobsOnInvalidRule(
            string filter,
            string email,
            int? state,
            int? type
        )
        {
            var filterDto = new FilterDTO(filter, email, state, type);

            _ = _repo
                .Setup(repo => repo.Filter(It.IsAny<IJobFilterStrategy>()))
                .ReturnsAsync(new List<Job>());

            var result = await _service.GetByFilter(filterDto);

            // Assert
            Assert.That(result, Is.Not.Null);
            Assert.That(result, Is.Empty);

            // Verify interactions
            _repo.Verify(repo => repo.Filter(It.IsAny<IJobFilterStrategy>()), Times.Once);
            _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [TestCase("TYPE", null, null, 0)]
        [TestCase("TYPE", null, null, 1)]
        public async Task GetByFilterWithTypeFilterReturnsFilteredJobs(
            string filter,
            string email,
            int? state,
            int? type
        )
        {
            // Arrange
            var filterDto = new FilterDTO(filter, email, state, type);
            var deliveries = new List<Job>
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
                    "test1@isep.ipp.pt",
                    new JobLocation(new Coordinates("C", 2, 2, 3), new Coordinates("D", 2, 10, 14)),
                    new JobContact(),
                    new JobContact(),
                    new JobConfirmationCode(43311),
                    "Test description"
                )
            };

            var surveillances = new List<Job>
            {
                new JobSurveillance(
                    "test2@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact()
                ),
                new JobSurveillance(
                    "test3@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact()
                )
            };

            (var matchingJobs, var nonMatchingJobs) = type switch
            {
                0 => (deliveries, surveillances),
                1 => (surveillances, deliveries),
                _ => throw new AssertionException("bad type"),
            };

            _ = _repo
                .Setup(repo => repo.Filter(It.IsAny<IJobFilterStrategy>()))
                .ReturnsAsync(matchingJobs);

            // Act
            var result = await _service.GetByFilter(filterDto);

            // Assert
            Assert.That(result, Is.Not.Null);
            CollectionAssert.AreEqual(matchingJobs, result);

            // result must not have members from the nonMatchingJobs
            var nonMatchingOverlap = result.Intersect(nonMatchingJobs);
            Assert.That(nonMatchingOverlap, Is.Empty);

            // Verify interactions
            _repo.Verify(repo => repo.Filter(It.IsAny<JobDeviceFilterStrategy>()), Times.Once);
            _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [TestCase("STATE", null, 1, null)]
        public async Task GetByFilterWithStateFilterReturnsFilteredJobs(
            string filter,
            string email,
            int? state,
            int? type
        )
        {
            // Arrange
            var filterDto = new FilterDTO(filter, email, state, type);

            // Create a job with an initial state that allows transitioning to the desired state
            var initialJob = new JobSurveillance(
                "test1@isep.ipp.pt",
                new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                new JobContact()
            );
            _ = initialJob.Update(new JobUpdateProps { Status = JobStateEnum.APPROVED });

            var matchingJobs = new List<Job> { initialJob };

            // Create non-matching jobs with various states
            var nonMatchingJobs = new List<Job>
            {
                new JobSurveillance(
                    "test2@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact()
                ),
                new JobDelivery(
                    "test3@isep.ipp.pt",
                    new JobLocation(new Coordinates("B", 1, 2, 3), new Coordinates("B", 1, 2, 3)),
                    new JobContact(),
                    new JobContact(),
                    new JobConfirmationCode(33311),
                    "Test description"
                )
            };

            _ = _repo
                .Setup(repo => repo.Filter(It.IsAny<IJobFilterStrategy>()))
                .ReturnsAsync(matchingJobs);

            // Act
            var result = await _service.GetByFilter(filterDto);

            // Assert
            Assert.That(result, Is.Not.Null);
            CollectionAssert.AreEqual(matchingJobs, result);

            // result must not have members from the nonMatchingJobs
            var nonMatchingOverlap = result.Intersect(nonMatchingJobs);
            Assert.That(nonMatchingOverlap, Is.Empty);

            // Verify interactions
            _repo.Verify(repo => repo.Filter(It.IsAny<JobStateFilterStrategy>()), Times.Once);
            _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
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

            _planning
                .Setup(planning => planning.ComputeSequence(It.IsAny<ComputeSequenceDto>()))
                .ReturnsAsync(taskSequenceDto);

            _repo
                .Setup(repo => repo.GetByIdAsync(It.IsAny<JobId>()))
                .ReturnsAsync((JobId jobId) => jobs.FirstOrDefault(j => j.Id == jobId));

            _repo.Setup(repo => repo.Update(It.IsAny<Job>())).ReturnsAsync((Job job) => job);

            _sequence.Setup(repo => repo.AddAsync(It.IsAny<Sequence>())).Verifiable();

            var result = await _service.JobSequence(dto);

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
        }

        [Test]
        public async Task UpdateJobSucceedsWithRigthParameters()
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

            var result = await _service.UpdateJob(dto);

            // Assert
            Assert.That(result, Is.Not.Null);
            Assert.That(result.Status, Is.EqualTo(JobStateEnum.APPROVED));

            // // Verify interactions
            _repo.Verify(repo => repo.GetByIdAsync(It.Is<JobId>(id => id.Value == dto.JobId)), Times.Once);
            _repo.Verify(repo => repo.Update(It.Is<Job>(j => j.Email == job.Email)), Times.Once);
            _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
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

            Assert.ThrowsAsync<BusinessRuleValidationException>(async () => await _service.UpdateJob(dto));
        }

        [Test]
        public async Task UpdateJobFailsIfJobNotFound()
        {
            var dto = new UpdatingJobDto
            {
                JobId = "01268906-2204-459d-8e86-335839f4e413",
                JobStatus = "Approved"
            };

            _ = _repo
                .Setup(repo => repo.GetByIdAsync(It.IsAny<JobId>()))
                .ReturnsAsync(() => null);

            Assert.ThrowsAsync<NotFoundException>(async () => await _service.UpdateJob(dto));
        }

        // avoid 'might be null' warnings
        public JobServiceTest()
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
    }
}

