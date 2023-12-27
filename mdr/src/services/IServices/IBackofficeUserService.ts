import { Either } from '../../core/logic/Result'
import { ICreatedBackofficeUserDTO } from '../../dto/ICreatedBackofficeUserDTO'
import { IBackofficeUserDTO } from '../../dto/IBackofficeUserDTO'

export enum BackofficeUserErrorCode {
    NotFound,
    AlreadyExists,
    BussinessRuleViolation,
}

export type BackofficeUserErrorResult = {
    errorCode: BackofficeUserErrorCode
    message: string
}

export default interface IBackofficeUserService {
    createBackofficeUser(
        backofficeUserDTO: IBackofficeUserDTO,
    ): Promise<Either<BackofficeUserErrorResult, ICreatedBackofficeUserDTO>>
    getUser(dto: {
        email: string
    }): Promise<Either<BackofficeUserErrorResult, ICreatedBackofficeUserDTO>>
}
