import { Result } from '../../core/logic/Result'
import { IBuildingCodeDTO } from '../../dto/IBuildingCodeDTO'
import { IFloorDTO } from '../../dto/IFloorDTO'
import { IFloorMapDTO } from '../../dto/IFloorMapDTO'
import { IFloorPassageDTO } from '../../dto/IFloorPassageDTO'
import { IUpdateFloorDTO } from '../../dto/IUpdateFloorDTO'

export default interface IFloorService {
    createFloor(floorDTO: IFloorDTO, buildingId: string): Promise<Result<IFloorDTO>>
    patchFloor(dto: IUpdateFloorDTO): Promise<Result<IFloorDTO>>
    putFloor(dto: IUpdateFloorDTO): Promise<Result<IFloorDTO>>
    getFloors(buildingCode: string): Promise<Result<IFloorDTO[]>>
    floorsWithPassage(buildingDTO: IBuildingCodeDTO): Promise<Result<IFloorPassageDTO[]>>
    uploadMap(floorMapDto: IFloorMapDTO): Promise<Result<IFloorDTO>>
}
