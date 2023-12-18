import { Either } from '../../core/logic/Result'
import { IClientDTO } from '../../dto/IClientDTO'
import { ICreatedClientDTO } from '../../dto/ICreatedClientDTO'

export enum ClientErrorCode {
    NotFound,
    BussinessRuleViolation,
}

export type ClientErrorResult = {
    errorCode: ClientErrorCode
    message: string
}

export default interface IClientService {
    createClient(clientDTO: IClientDTO): Promise<Either<ClientErrorResult, ICreatedClientDTO>>
}
