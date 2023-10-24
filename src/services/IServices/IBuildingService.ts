import { Result } from '../../core/logic/Result'
import { IBuildingDTO } from '../../dto/IBuildingDTO'
import { IBuildingMinMaxFloorsDTO } from '../../dto/IBuildingMinMaxFloorsDTO'

export default interface IBuildingService {
    createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>
    getBuilding(buildingId: string): Promise<Result<IBuildingDTO>>
    getBuildings(): Promise<Result<IBuildingDTO[]>>
    getBuildingsByFloors(minMaxFloorsDTO: IBuildingMinMaxFloorsDTO): Promise<Result<IBuildingDTO[]>>
    editBuilding(buildingId: string, buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>
}
