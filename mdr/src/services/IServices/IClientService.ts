import { Either } from '../../core/logic/Result'
import { IClientDTO } from '../../dto/IClientDTO'
import { IClientWithoutPasswordDTO } from '../../dto/IClientWithoutPasswordDTO'
import { ICreatedClientDTO } from '../../dto/ICreatedClientDTO'
import {IDeletedClientDTO} from "../../dto/IDeletedClientDTO";

export enum ClientErrorCode {
    NotFound,
    AlreadyExists,
    BussinessRuleViolation,
}

export type ClientErrorResult = {
    errorCode: ClientErrorCode
    message: string
}

export default interface IClientService {
    createClient(
        clientDTO: IClientDTO,
    ): Promise<Either<ClientErrorResult, ICreatedClientDTO>>
    getClient(
        email: string,
    ): Promise<Either<ClientErrorResult, IClientWithoutPasswordDTO>>
    deleteClient(
        createdClientDTO: ICreatedClientDTO,
    ): Promise<Either<ClientErrorResult, IDeletedClientDTO>>
}
