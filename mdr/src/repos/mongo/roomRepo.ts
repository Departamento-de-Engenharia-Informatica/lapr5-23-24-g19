import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import IRoomRepo from '../../services/IRepos/IRoomRepo'
import { IRoomPersistence } from '../../dataschema/mongo/IRoomPersistence'
import Room from '../../domain/room/room'
import { RoomMap } from '../../mappers/RoomMap'
import Building from '../../domain/building/building'
import { Floor } from '../../domain/floor/floor'
import { BuildingCode } from '../../domain/building/code'
import { FloorNumber } from '../../domain/floor/floorNumber'
import { RoomName } from '../../domain/room/roomName'

@Service()
export default class RoomRepo implements IRoomRepo {
    private models: any

    constructor(
        @Inject('roomSchema') private roomSchema: Model<IRoomPersistence & Document>,
    ) {}
    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(room: Room): Promise<boolean> {
        const query = { name: room.name.value }
        const buildingDocument = await this.roomSchema.findOne(query)

        return buildingDocument != null
    }

    public async save(room: Room): Promise<Room> {
        const query = { name: room.name.value }

        const roomDocument = await this.roomSchema.findOne(query)

        try {
            const rawFloor = RoomMap.toPersistence(room)
            if (await this.exists(room)) {
                roomDocument.name = rawFloor.name
                roomDocument.buildingCode = rawFloor.buildingCode
                roomDocument.floorNumber = rawFloor.floorNumber
                roomDocument.description = rawFloor.description
                roomDocument.dimensions = rawFloor.dimensions
                roomDocument.position = rawFloor.position

                await roomDocument.save()
                return RoomMap.toDomain(roomDocument)
            }
            const rawPassage = RoomMap.toPersistence(room)
            const passageCreated = await this.roomSchema.create(rawPassage)

            return RoomMap.toDomain(passageCreated)
        } catch (err) {
            throw err
        }
    }

    async findAllInFloor(building: Building, floor: Floor): Promise<Room[]> {
        const query = {
            buildingCode: building.code.value,
            floorNumber: floor.floorNumber.value,
        }

        const records = await this.roomSchema.find(query)

        if (records.length === 0) {
            return []
        }
        return await Promise.all(records.map((record) => RoomMap.toDomain(record)))
    }

    async find(
        building: BuildingCode,
        floor: FloorNumber,
        name: RoomName,
    ): Promise<Room> {
        const doc = await this.roomSchema.findOne({
            buildingCode: building.value,
            floorNumber: floor.value,
            name: name.value,
        })

        if (!doc) {
            return null
        }

        return RoomMap.toDomain(doc)
    }
}
