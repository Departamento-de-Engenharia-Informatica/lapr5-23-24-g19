import { Repo } from '../../core/infra/Repo'
import Building from '../../domain/building/building'
import { FloorNumber } from '../../domain/floor/floorNumber'
import { BuildingCode } from '../../domain/building/code'
import { Floor } from '../../domain/floor/floor'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

export type BuildingFloorCount = {
    buildingCode: BuildingCode
    floorCount: number
}

export default interface IFloorRepo extends Repo<Floor> {
    save(floor: Floor): Promise<Floor>
    exists(floor: Floor | string): Promise<boolean>
    find(building: Building, floorNumber: FloorNumber): Promise<Floor>
    findByBuildingCode(buildingCode: BuildingCode): Promise<Floor[]>
    findByCodeNumber(buildingCode: BuildingCode, floorNumber: FloorNumber): Promise<Floor>
    findBuildingsByMinMaxFloors(min: number, max: number): Promise<BuildingFloorCount[]>
    findAllInBuilding(building: Building): Promise<Floor[]>
    findById(id: UniqueEntityID | string): Promise<Floor>
}
