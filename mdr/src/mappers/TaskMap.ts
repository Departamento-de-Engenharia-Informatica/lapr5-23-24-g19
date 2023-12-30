import Container from 'typedi'
import { CreateDeliveryTaskDTO } from '../dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../dto/CreateSurveillanceTaskDTO'
import { BuildingCode } from '../domain/building/code'
import { FloorNumber } from '../domain/floor/floorNumber'
import { RoomName } from '../domain/room/roomName'
import { ICreateDeliveryTaskToMapperDTO } from '../dto/ICreateDeliveryTaskToMapperDTO'
import { IGeneralTaskDTO } from '../dto/IGeneralTaskDTO'
import { ITaskDTO } from '../dto/ITaskDTO'
import RoomRepo from '../repos/mongo/roomRepo'

export class TaskMap {
    public static toGeneralTaskDto(task: string): IGeneralTaskDTO {
        const dto = JSON.parse(JSON.stringify(task)) as IGeneralTaskDTO
        return dto
    }

    public static surveillanceDtoToTaskDto(
        dto: CreateSurveillanceTaskDTO,
        x: number,
        y: number,
    ): ITaskDTO {
        return {
            Email: dto.email,
            Location: {
                StartingPoint: {
                    BuildingCode: dto.buildingCode,
                    FloorNumber: dto.floorNumber,
                    X: x,
                    Y: y,
                },
                EndingPoint: {
                    BuildingCode: dto.buildingCode,
                    FloorNumber: dto.floorNumber,
                    X: x,
                    Y: y,
                },
            },
            JobType: 0,
            Surveillance: {
                Contact: {
                    Name: dto.contactName,
                    Phone: dto.contactPhone,
                },
            },
        } as ITaskDTO
    }

    public static async deliveryDtoToTaskDto(
        dto: ICreateDeliveryTaskToMapperDTO,
    ): Promise<ITaskDTO> {
        console.log(dto)
        return {
            Email: dto.email,
            Location: {
                StartingPoint: {
                    BuildingCode: dto.startBuildingCode,
                    FloorNumber: dto.startFloorNumber,
                    X: dto.startRoom.x,
                    Y: dto.startRoom.y,
                },

                EndingPoint: {
                    BuildingCode: dto.goalBuildingCode,
                    FloorNumber: dto.goalFloorNumber,
                    X: dto.goalRoom.x,
                    Y: dto.goalRoom.y,
                },
            },
            JobType: 1,
            Delivery: {
                PickupContact: {
                    Name: dto.pickupContactName,
                    Phone: dto.pickupContactPhone,
                },
                DeliveryContact: {
                    Name: dto.deliveryContactName,
                    Phone: dto.deliveryContactPhone,
                },
                Description: dto.description,
                ConfirmationCode: dto.confirmationCode,
            },
        } as ITaskDTO
    }
}
