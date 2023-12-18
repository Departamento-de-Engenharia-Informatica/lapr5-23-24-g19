import Container from 'typedi'
import { CreateDeliveryTaskDTO } from '../../../spa/src/app/dto/CreateDeliveryTaskDTO'
import { CreateSurveillanceTaskDTO } from '../../../spa/src/app/dto/CreateSurveillanceTaskDTO'
import { BuildingCode } from '../domain/building/code'
import { FloorNumber } from '../domain/floor/floorNumber'
import { RoomName } from '../domain/room/roomName'
import { ITaskDTO } from '../dto/ITaskDTO'
import RoomRepo from '../repos/mongo/roomRepo'

export class TaskMap {
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
        dto: CreateDeliveryTaskDTO,
    ): Promise<ITaskDTO> {
        const roomRepo = Container.get(RoomRepo)

        const startRoom = await roomRepo.find(
            BuildingCode.create(dto.startBuildingCode).getValue(),
            FloorNumber.create(dto.startFloorNumber).getValue(),
            RoomName.create(dto.startRoom).getValue(),
        )

        const goalRoom = await roomRepo.find(
            BuildingCode.create(dto.goalBuildingCode).getValue(),
            FloorNumber.create(dto.goalFloorNumber).getValue(),
            RoomName.create(dto.goalRoom).getValue(),
        )

        return {
            Email: dto.email,
            Location: {
                StartingPoint: {
                    BuildingCode: dto.startBuildingCode,
                    FloorNumber: dto.startFloorNumber,
                    X: startRoom.positions.x,
                    Y: startRoom.positions.y,
                },

                EndingPoint: {
                    BuildingCode: dto.goalBuildingCode,
                    FloorNumber: dto.goalFloorNumber,
                    X: goalRoom.positions.x,
                    Y: goalRoom.positions.y,
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
