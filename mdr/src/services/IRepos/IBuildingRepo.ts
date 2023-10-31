import { Repo } from '../../core/infra/Repo'
import Building from '../../domain/building/building'
import { BuildingCode } from '../../domain/building/code'

export default interface IBuildingRepo extends Repo<Building> {
    save(building: Building): Promise<Building>
    findByCode(code: BuildingCode): Promise<Building>
    findAll(): Promise<Building[]>
}
