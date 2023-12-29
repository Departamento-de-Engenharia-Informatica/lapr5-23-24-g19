import { Either } from '../../core/logic/Result'
import { IClientDTO } from '../../dto/IClientDTO'
import { IClientWithoutPasswordDTO } from '../../dto/IClientWithoutPasswordDTO'
import { ICreatedClientDTO } from '../../dto/ICreatedClientDTO'
import { IDeletedClientDTO } from '../../dto/IDeletedClientDTO'
import IUpdateClientStateDTO from '../../dto/IUpdateClientStateDTO'
import { IClientEmailDTO } from '../../dto/IClientEmailDTO'
import { IClientDataRequestDTO } from '../../dto/IClientDataRequestDTO'
import { IClientDataDTO } from '../../dto/IClientDataDTO'

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
    updateClientState(
        dto: IUpdateClientStateDTO,
    ): Promise<Either<ClientErrorResult, IClientWithoutPasswordDTO>>
    getClient(
        email: string,
    ): Promise<Either<ClientErrorResult, IClientWithoutPasswordDTO>>
    getClientsByState(
        state: string,
    ): Promise<Either<ClientErrorResult, ICreatedClientDTO[]>>
    patchClient(
        clientDTO: IClientWithoutPasswordDTO,
    ): Promise<Either<ClientErrorResult, IClientWithoutPasswordDTO>>
    deleteClient(
        clientDTO: IClientEmailDTO,
    ): Promise<Either<ClientErrorResult, IDeletedClientDTO>>
    getClientData(
        dto: IClientDataRequestDTO,
    ): Promise<Either<ClientErrorResult, IClientDataDTO>>
}
