import { Either, Result } from '../../core/logic/Result'
import { IBuildingDTO } from '../../dto/IBuildingDTO'
import { IBuildingEditDTO } from '../../dto/IBuildingEditDTO'
import { IBuildingMinMaxFloorsDTO } from '../../dto/IBuildingMinMaxFloorsDTO'

export enum ErrorCode {
    NotFound,
    RequirementsNotMatch,
    AlreadyExists,
    BusinessRuleViolation,
}

export type ErrorResult = {
    errorCode: ErrorCode
    message: string
}

export default interface IBuildingService {
    createBuilding(buildingDTO: IBuildingDTO): Promise<Either<ErrorResult, IBuildingDTO>>
    getBuilding(buildingId: string): Promise<Either<ErrorResult, IBuildingDTO>>
    getBuildings(): Promise<Either<ErrorResult, IBuildingDTO[]>>
    getBuildingsByFloors(
        minMaxFloorsDTO: IBuildingMinMaxFloorsDTO,
    ): Promise<Either<ErrorResult, IBuildingDTO[]>>
    editBuilding(
        buildingDTO: IBuildingEditDTO,
    ): Promise<Either<ErrorResult, IBuildingDTO>>
}
