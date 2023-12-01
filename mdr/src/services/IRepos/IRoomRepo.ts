import { Repo } from '../../core/infra/Repo'
import Room from '../../domain/room/room'
import Building from '../../domain/building/building'
import { Floor } from '../../domain/floor/floor'
import { BuildingCode } from '../../domain/building/code'
import { FloorNumber } from '../../domain/floor/floorNumber'
import { RoomName } from '../../domain/room/roomName'

export default interface IRoomRepo extends Repo<Room> {
    save(room: Room): Promise<Room>
    findAllInFloor(building: Building, floor: Floor): Promise<Room[]>
    find(building: BuildingCode, floor: FloorNumber, name: RoomName): Promise<Room>
}
