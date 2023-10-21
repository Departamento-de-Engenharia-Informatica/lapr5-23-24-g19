import { Result } from '../../core/logic/Result'
import { IFloorDTO } from '../../dto/IFloorDTO'

export default interface IFloorService {
    createFloor(floorDTO: IFloorDTO, buildingId: string): Promise<Result<IFloorDTO>>
}
