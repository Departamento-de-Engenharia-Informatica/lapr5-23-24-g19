using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Jobs.DTO;
using DDDSample1.Util.Coordinates;

namespace DDDSample1.Domain.Jobs.Mapper
{
    public class JobMapper
    {
        public JobMapper() { }

        public static Job ToDomain(CreatingJobDto dto)
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

            return job;
        }
    }
}
