using DDDSample1.Domain.Products;
using DDDSample1.Domain.Shared;
using DDDSample1.Util.Coordinates;
using System;
using System.Text.Json;
using System.Threading.Tasks;

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
            // var fam = await this._repo.GetByIdAsync(id);
            var job = await this._repo.GetByIdAsync(new JobId(id));
            
            if(job == null)
                return null;

            var options = new JsonSerializerOptions
            {
                WriteIndented = true // This sets the indentation
            };
            return JsonSerializer.Serialize(job,options);
        }


        public async Task<CreatingJobDto> AddAsync(CreatingJobDto dto)
        {
            Job job = dto.Type switch
            {
                0 => new JobSurveillance(dto.Email,
                    new JobLocation(new Coordinates(dto.Location.StartingPoint.BuildingCode, dto.Location.StartingPoint.FloorNumber, dto.Location.StartingPoint.X, dto.Location.StartingPoint.Y),
                                    new Coordinates(dto.Location.EndingPoint.BuildingCode, dto.Location.EndingPoint.FloorNumber, dto.Location.EndingPoint.X, dto.Location.EndingPoint.Y)),
                                    new JobContact(dto.Surveillance.Contact.Name, dto.Surveillance.Contact.Phone)),

                1 => new JobDelivery(dto.Email,
                    new JobLocation(new Coordinates(dto.Location.StartingPoint.BuildingCode, dto.Location.StartingPoint.FloorNumber, dto.Location.StartingPoint.X, dto.Location.StartingPoint.Y),
                                    new Coordinates(dto.Location.EndingPoint.BuildingCode, dto.Location.EndingPoint.FloorNumber, dto.Location.EndingPoint.X, dto.Location.EndingPoint.Y)),
                                    new JobContact(dto.Delivery.PickupContact.Name, dto.Delivery.PickupContact.Phone),
                                    new JobContact(dto.Delivery.DeliveryContact.Name, dto.Delivery.DeliveryContact.Phone),
                                    dto.Delivery.Description),

                _ => throw new ArgumentException("Invalid job type")
            };

            await this._repo.AddAsync(job);
            await this._unitOfWork.CommitAsync();

            return dto;
        }

        // public async Task<JobDto> AddAsync(CreatingJobDto dto)
        // {
        //     Job job = dto.Type switch
        //     {
        //         0 => new JobSurveillance(dto.Email,
        //             new JobLocation(new Coordinates(dto.Location.StartingPoint.BuildingCode, dto.Location.StartingPoint.FloorNumber, dto.Location.StartingPoint.X, dto.Location.StartingPoint.Y),
        //                             new Coordinates(dto.Location.EndingPoint.BuildingCode, dto.Location.EndingPoint.FloorNumber, dto.Location.EndingPoint.X, dto.Location.EndingPoint.Y)),
        //                             new JobContact(dto.Surveillance.Contact.Name, dto.Surveillance.Contact.PhoneNumber)),

        //         1 => new JobDelivery(dto.Email,
        //             new JobLocation(new Coordinates(dto.Location.StartingPoint.BuildingCode, dto.Location.StartingPoint.FloorNumber, dto.Location.StartingPoint.X, dto.Location.StartingPoint.Y),
        //                             new Coordinates(dto.Location.EndingPoint.BuildingCode, dto.Location.EndingPoint.FloorNumber, dto.Location.EndingPoint.X, dto.Location.EndingPoint.Y)),
        //                             new JobContact(dto.Delivery.PickupContact.Name, dto.Delivery.PickupContact.PhoneNumber),
        //                             new JobContact(dto.Delivery.DeliveryContact.Name, dto.Delivery.DeliveryContact.PhoneNumber),
        //                             dto.Delivery.Description),

        //         _ => throw new ArgumentException("Invalid job type")
        //     };

        //     await this._repo.AddAsync(job);
        //     await this._unitOfWork.CommitAsync();

        //     return new JobDto(job.Email, new JobLocationDto(new CoordinatesDto(job.Location.StartingPoint.BuildingCode, job.Location.StartingPoint.FloorNumber, job.Location.StartingPoint.X, job.Location.StartingPoint.Y),
        //                                                     new CoordinatesDto(job.Location.EndingPoint.BuildingCode, job.Location.EndingPoint.FloorNumber, job.Location.EndingPoint.X, job.Location.EndingPoint.Y)),
        //                                                     job.Status.ToString(), JobType.ToCode(job.Type), 
                                                        
        //                                                     job.Delivery != null ? new JobDeliveryDto(new JobContactDto(job.Delivery.PickupContact.Name, job.Delivery.PickupContact.PhoneNumber),
        //                                                                                                                          new JobContactDto(job.Delivery.DeliveryContact.Name, job.Delivery.DeliveryContact.PhoneNumber),
        //                                                                                                                          job.Delivery.Description) : null,
                                                                                                                                 
        //                                                     job.Surveillance != null ? new JobSurveillanceDto(new JobContactDto(job.Surveillance.Contact.Name, job.Surveillance.Contact.PhoneNumber)) : null);
        // }
    }    
}