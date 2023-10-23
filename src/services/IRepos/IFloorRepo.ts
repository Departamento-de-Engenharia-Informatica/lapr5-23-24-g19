import { Repo } from '../../core/infra/Repo'
import Building from '../../domain/building/building'
import { Floor } from '../../domain/floor/floor'
import { FloorId } from '../../domain/floor/floorId'
import { FloorNumber } from '../../domain/floor/floorNumber'

export default interface IFloorRepo extends Repo<Floor> {
    save(floor: Floor): Promise<Floor>
    exists(floor: Floor | string): Promise<boolean>
    find(building: Building, floorNumber: FloorNumber): Promise<Floor>
}
