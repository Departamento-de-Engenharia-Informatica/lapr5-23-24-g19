import { Either, Result } from '../../core/logic/Result'
import { ICreatedElevatorDTO } from '../../dto/ICreatedElevatorDTO'
import { IElevatorDTO } from '../../dto/IElevatorDTO'

export enum ErrorCode {
    NotFound,
    BussinessRuleViolation,
}

export type ErrorResult = {
    errorCode: ErrorCode
    message: string
}

export default interface IElevatorService {
    createElevator(
        elevatorDto: IElevatorDTO,
    ): Promise<Either<ErrorResult, ICreatedElevatorDTO>>
    editElevator(
        identifier: number,
        elevatorDto: IElevatorDTO,
    ): Promise<Either<ErrorResult, ICreatedElevatorDTO>>
    getElevators(code: string): Promise<Either<ErrorResult, ICreatedElevatorDTO[]>>
}
