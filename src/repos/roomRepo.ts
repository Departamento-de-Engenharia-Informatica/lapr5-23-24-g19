import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import IRoomRepo from '../services/IRepos/IRoomRepo'
import { IRoomPersistence } from '../dataschema/IRoomPersistence'
import Room from '../domain/room/room'
import { RoomId } from '../domain/room/roomId'

@Service()
export default class RoomRepo implements IRoomRepo {
    private models: any

    constructor(@Inject('roomSchema') private roomSchema: Model<IRoomPersistence & Document>) {}

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }
    public async exists(room: Room): Promise<boolean> {
        const idX = room instanceof RoomId ? room.id.toValue() : room

        const query = { domainId: idX }
        const buildingDocument = await this.roomSchema.findOne(query)

        return !!buildingDocument === true
    }
    public async save(room: Room): Promise<Room> {
        return room
    }
}
