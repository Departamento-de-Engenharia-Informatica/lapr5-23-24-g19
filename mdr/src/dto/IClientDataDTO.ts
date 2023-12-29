import { IClientTaskDTO } from './IClientTaskDTO'

export interface IClientDataDTO {
    accountData: {
        name: string
        email: string
        phoneNumber: string
        vatNumber: string
        status: string
    }
    requestedTasks: {
        tasks: IClientTaskDTO[]
    }
}
