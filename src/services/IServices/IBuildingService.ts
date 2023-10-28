import { Either, Result } from '../../core/logic/Result'
import { IBuildingDTO } from '../../dto/IBuildingDTO'
import { IBuildingMinMaxFloorsDTO } from '../../dto/IBuildingMinMaxFloorsDTO'

export enum ErrorCode{
    NotFound,
    BussinessRuleViolation
}

export type ErrorResult = {
    errorCode: ErrorCode
    message: string
}


export default interface IBuildingService {
    createBuilding(buildingDTO: IBuildingDTO): Promise<Either<ErrorResult,IBuildingDTO>>
    getBuilding(buildingId: string): Promise<Result<IBuildingDTO>>
    getBuildings(): Promise<Result<IBuildingDTO[]>>
    getBuildingsByFloors(minMaxFloorsDTO: IBuildingMinMaxFloorsDTO): Promise<Result<IBuildingDTO[]>>
    editBuilding(buildingId: string, buildingDTO: IBuildingDTO): Promise<Either<ErrorResult,IBuildingDTO>>
}
