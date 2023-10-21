import { Repo } from '../../core/infra/Repo'
import { Floor } from '../../domain/floor/floor'
import { FloorId } from '../../domain/floor/floorId'

export default interface IFloorRepo extends Repo<Floor> {
    save(floor: Floor): Promise<Floor>
    exists(floor: Floor | string): Promise<boolean>
}
