import { Repo } from '../../core/infra/Repo'
import { Building } from '../../domain/building'
import { BuildingId } from '../../domain/buildingId'

export default interface IBuildingRepo extends Repo<Building> {
    save(building: Building): Promise<Building>
    findById(id: string): Promise<Building>
    exists(buildingId: BuildingId | string): Promise<boolean>
}
