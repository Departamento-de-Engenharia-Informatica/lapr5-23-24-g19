using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Threading.Tasks;
using DDDSample1.Domain.Jobs.Filter;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;

namespace DDDSample1.Domain.Jobs
{
    public class JobService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IJobRepository _repo;

        public JobService(IUnitOfWork unitOfWork, IJobRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<String> GetByIdAsync(String id)
        {
            var job = await this._repo.GetByIdAsync(new JobId(id));

            if (job == null)
                return null;

            var options = new JsonSerializerOptions
            {
                WriteIndented = true // This sets the indentation
            };

            return JsonSerializer.Serialize(job, options);
        }

        public async Task<CreatingJobDto> AddAsync(CreatingJobDto dto)
        {
            Job job = dto.JobType switch
            {
                0
                    => new JobSurveillance(
                        dto.Email,
                        new JobLocation(
                            new Coordinates(
                                dto.Location.StartingPoint.BuildingCode,
                                dto.Location.StartingPoint.FloorNumber,
                                dto.Location.StartingPoint.X,
                                dto.Location.StartingPoint.Y
                            ),
                            new Coordinates(
                                dto.Location.EndingPoint.BuildingCode,
                                dto.Location.EndingPoint.FloorNumber,
                                dto.Location.EndingPoint.X,
                                dto.Location.EndingPoint.Y
                            )
                        ),
                        new JobContact(
                            dto.Surveillance.Contact.Name,
                            dto.Surveillance.Contact.Phone
                        )
                    ),

                1
                    => new JobDelivery(
                        dto.Email,
                        new JobLocation(
                            new Coordinates(
                                dto.Location.StartingPoint.BuildingCode,
                                dto.Location.StartingPoint.FloorNumber,
                                dto.Location.StartingPoint.X,
                                dto.Location.StartingPoint.Y
                            ),
                            new Coordinates(
                                dto.Location.EndingPoint.BuildingCode,
                                dto.Location.EndingPoint.FloorNumber,
                                dto.Location.EndingPoint.X,
                                dto.Location.EndingPoint.Y
                            )
                        ),
                        new JobContact(
                            dto.Delivery.PickupContact.Name,
                            dto.Delivery.PickupContact.Phone
                        ),
                        new JobContact(
                            dto.Delivery.DeliveryContact.Name,
                            dto.Delivery.DeliveryContact.Phone
                        ),
                        new JobConfirmationCode(dto.Delivery.ConfirmationCode),
                        dto.Delivery.Description
                    ),

                _ => throw new ArgumentException("Invalid job type")
            };

            await this._repo.AddAsync(job);
            await this._unitOfWork.CommitAsync();

            return dto;
        }

        public async Task<List<Job>> GetByFilter(FilterDTO dto)
        {
            var strategy = JobFilterContext.FilterStrategy(dto);
            var jobs = await _repo.Filter(strategy);
            _ = await _unitOfWork.CommitAsync(); // ??

            return jobs;


            //TOOD: strategy pattern
            // var jobs = new List<Job>();
            // switch (dto.Filter)
            // {
            //     case "STATE":
            //         if (dto.State.HasValue)
            //         {
            //             Console.WriteLine("dto");
            //             Console.WriteLine(dto.State.Value);
            //             jobs = await this._repo.GetByState(JobState.FromCode(dto.State.Value));
            //         }
            //         break;
            //     //TODO: TYPE OR LIST OF TYPES??
            //     case "TYPE":
            //         if (dto.Type.HasValue)
            //         {
            //             Console.WriteLine("dto");
            //             Console.WriteLine(dto.Type.Value);
            //             jobs = await this._repo.GetByType(JobType.FromCode(dto.Type.Value));
            //         }
            //         break;
            //     case "CLIENT":
            //         if (dto.Email.Length > 0)
            //         {
            //             jobs = await this._repo.GetByEmail(dto.Email);
            //         }
            //         break;
            //     default:
            //         //TODO: throw error?
            //         return null;
            // }
            // return jobs;
        }

    }

}
