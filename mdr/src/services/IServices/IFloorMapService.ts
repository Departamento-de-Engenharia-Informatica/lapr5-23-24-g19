import { Either } from '../../core/logic/Result'
import { IBuildingFloorDTO } from '../../dto/IBuildingFloorDTO'
import { IFloorDTO } from '../../dto/IFloorDTO'
import { IFloorMapDTO } from '../../dto/IFloorMapDTO'

export enum ErrorCode {
    NotFound,
    BusinessRuleViolation,
}

export type ErrorResult = {
    errorCode: ErrorCode
    message: string
}

export default interface IFloorMapService {
    getMap(dto: IBuildingFloorDTO): Promise<Either<ErrorResult, IFloorMapDTO>>
    uploadMap(floorMapDto: IFloorMapDTO): Promise<Either<ErrorResult, IFloorMapDTO>>
}
