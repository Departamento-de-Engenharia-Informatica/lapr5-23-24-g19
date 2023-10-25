import { Result } from '../../core/logic/Result'
import { IBuildingCodeDTO } from '../../dto/IBuildingCodeDTO'
import { IFloorDTO } from '../../dto/IFloorDTO'
import { IFloorMapDTO } from '../../dto/IFloorMapDTO'

export default interface IFloorService {
    createFloor(floorDTO: IFloorDTO, buildingId: string): Promise<Result<IFloorDTO>>
    getFloors(buildingCode: string): Promise<Result<IFloorDTO[]>>
    floorsWithPassage(buildingDTO: IBuildingCodeDTO): Promise<Result<IFloorDTO[]>>
    uploadMap(floorMapDto: IFloorMapDTO): Promise<Result<IFloorDTO>>
}
