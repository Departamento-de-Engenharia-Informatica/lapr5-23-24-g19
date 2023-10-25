import { Mapper } from '../core/infra/Mapper'
import { IRoomDTO } from '../dto/IRoomDTO'

import Room from '../domain/room/room'

export class RoomMap extends Mapper<Room> {
    public static toDTO(room: Room): IRoomDTO {
        const { length, width } = room.dimensions

        const { x_axis, y_axis } = room.positions

        return {
            buildingCode: room.building.code.value,
            floor: room.floor.floorNumber.value,
            name: room.name.value,
            description: room.description.value,
            category: room.category.value,
            roomDimensions: { length, width },
            roomPositions: { x_axis, y_axis },
        }
    }

    public static async toDomain(raw: any): Promise<Room> {
        return null
    }

    public static toPersistence(room: Room): any {
        return {
            domainId: room.id.toString(),
            buildingCode: room.building.code.value,
            floor: room.floor.floorNumber.value,
            name: room.name.value,
            description: room.description.value,
            category: room.category.value,
            roomDimensionLength: room.dimensions.length,
            roomDimensionWidth: room.dimensions.width,
            roomPositionX: room.positions.x_axis,
            roomPositionY: room.positions.y_axis,
        }
    }
}
