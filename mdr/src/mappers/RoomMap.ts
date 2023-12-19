import { Mapper } from '../core/infra/Mapper'
import { IRoomDTO } from '../dto/IRoomDTO'

import Room from '../domain/room/room'
import Container from 'typedi'
import FloorRepo from '../repos/mongo/floorRepo'
import { FloorNumber } from '../domain/floor/floorNumber'
import { BuildingCode } from '../domain/building/code'
import { RoomDescription } from '../domain/room/description'
import { RoomName } from '../domain/room/roomName'
import { Coordinates } from '../domain/floor/Coordinates'
import { RoomDimensions } from '../domain/room/roomDimensions'
import { RoomCategory } from '../domain/room/roomCategory'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'
import { IRoomPersistence } from '../dataschema/mongo/IRoomPersistence'

export class RoomMap extends Mapper<Room> {
    public static toDTO(room: Room): IRoomDTO {
        return {
            buildingCode: room.floor.building.code.value,
            floorNumber: room.floor.floorNumber.value,
            name: room.name.value,
            description: room.description.value,
            category: room.category.value,
            dimensions: {
                length: room.dimensions.length,
                width: room.dimensions.width,
            },
            positions: {
                x: room.positions.x,
                y: room.positions.y,
            },
        }
    }

    public static async toDomain(raw: IRoomPersistence): Promise<Room> {
        //TODO: implement map component toDomain
        const floorRepo = Container.get(FloorRepo)
        const floorFound = await floorRepo.findByCodeNumber(
            BuildingCode.create(raw.buildingCode).getValue(),
            FloorNumber.create(raw.floorNumber).getValue(),
        )
        const rawName = RoomName.create(raw.name).getValue()
        const rawDescription = RoomDescription.create(raw.description).getValue()
        const rawCategory = RoomCategory.create(raw.category).getValue()
        const rawDimensions = RoomDimensions.create(
            raw.dimensions.length,
            raw.dimensions.width,
        ).getValue()
        const rawPositions = Coordinates.create(raw.position.x, raw.position.y).getValue()

        const roomOrError = Room.create(
            {
                floor: floorFound,
                name: rawName,
                category: rawCategory,
                dimensions: rawDimensions,
                positions: rawPositions,
                description: rawDescription,
            },
            new UniqueEntityID(raw.domainId),
        )

        return roomOrError.isSuccess ? roomOrError.getValue() : null
    }

    public static toPersistence(room: Room): IRoomPersistence {
        return {
            domainId: room.id.toString(),
            buildingCode: room.floor.building.code.value,
            floorNumber: room.floor.floorNumber.value,
            name: room.name.value,
            description: room.description.value,
            category: room.category.value,
            dimensions: {
                length: room.dimensions.length,
                width: room.dimensions.width,
            },
            position: {
                x: room.positions.x,
                y: room.positions.y,
            },
        }
    }
}
