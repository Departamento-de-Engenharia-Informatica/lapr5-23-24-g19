import DataMapper from '../../core/infra/DataMapper'
import { Repo } from '../../core/infra/Repo'
import Building from '../../domain/building/building'
import Elevator from '../../domain/elevator/Elevator'
import { ElevatorIdentifier } from '../../domain/elevator/identifier'

export interface ElevatorDataMap<Persistence> extends DataMapper<Elevator, Persistence> {}

export default interface IElevatorRepo extends Repo<Elevator> {
    inBuilding(building: Building): Promise<Elevator[]>
    existsInBuilding(building: Building, identifier: ElevatorIdentifier): Promise<boolean>
    findByIdentifier(
        building: Building,
        identifier: ElevatorIdentifier,
    ): Promise<Elevator>
    nextIdentifier(): Promise<ElevatorIdentifier>
}
