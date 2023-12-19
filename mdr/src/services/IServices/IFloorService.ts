import { Either, Result } from '../../core/logic/Result'
import { IBuildingCodeDTO } from '../../dto/IBuildingCodeDTO'
import { IFloorDTO } from '../../dto/IFloorDTO'
import { IFloorPassageDTO } from '../../dto/IFloorPassageDTO'
import { IUpdateFloorDTO } from '../../dto/IUpdateFloorDTO'

export enum ErrorCode {
    NotFound,
    BusinessRuleViolation,
}

export type ErrorResult = {
    errorCode: ErrorCode
    message: string
}

export default interface IFloorService {
    createFloor(
        floorDTO: IFloorDTO,
        buildingId: string,
    ): Promise<Either<ErrorResult, IFloorDTO>>
    patchFloor(dto: IUpdateFloorDTO): Promise<Either<ErrorResult, IFloorDTO>>
    putFloor(dto: IUpdateFloorDTO): Promise<Either<ErrorResult, IFloorDTO>>
    getFloors(buildingCode: string): Promise<Either<ErrorResult, IFloorDTO[]>>
    floorsWithPassage(buildingDTO: IBuildingCodeDTO): Promise<Result<IFloorPassageDTO[]>>
}
