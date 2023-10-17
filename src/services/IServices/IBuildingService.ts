import { Result } from '../../core/logic/Result'
import { IBuildingDTO } from '../../dto/IBuildingDTO'

export default interface IBuildingService {
    createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>
    getBuilding(buildingId: string): Promise<Result<IBuildingDTO>>
}
