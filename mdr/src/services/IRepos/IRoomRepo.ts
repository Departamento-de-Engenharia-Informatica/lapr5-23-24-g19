import { Repo } from '../../core/infra/Repo'
import Room from '../../domain/room/room'
import Building from '../../domain/building/building'
import { Floor } from '../../domain/floor/floor'

export default interface IRoomRepo extends Repo<Room> {
    save(room: Room): Promise<Room>
    findAllInFloor(building: Building, floor: Floor): Promise<Room[]>
}
