import { UniqueEntityID } from '../core/domain/UniqueEntityID'
import { Mapper } from '../core/infra/Mapper'
import { IClientPersistence } from '../dataschema/mongo/IClientPersistence'
import Client from '../domain/user/client/Client'
import { ClientStatus } from '../domain/user/client/status'
import { VatNumber } from '../domain/user/client/vatNumber'
import { Email } from '../domain/user/email'
import { Name } from '../domain/user/name'
import { PhoneNumber } from '../domain/user/phoneNumber'
import { UserPassword } from '../domain/user/userPassword'
import { IClientDataDTO } from '../dto/IClientDataDTO'
import { IClientTaskDTO } from '../dto/IClientTaskDTO'
import { ICreatedClientDTO } from '../dto/ICreatedClientDTO'

export class ClientMap extends Mapper<Client> {
    static toDTO(client: Client): ICreatedClientDTO {
        return {
            name: client.name.value,
            email: client.email.value,
            phoneNumber: client.phoneNumber.value,
            vatNumber: client.vatNumber.value,
            status: client.status,
        }
    }

    static toPersistence(client: Client): IClientPersistence {
        return {
            domainId: client.id.toString(),
            name: client.name.value,
            email: client.email.value,
            phoneNumber: client.phoneNumber.value,
            vatNumber: client.vatNumber.value,
            status: client.status,

            password: client.props.password.value,
        }
    }

    static toDomain(raw: IClientPersistence): Client {
        const email = Email.create(raw.email).getOrThrow()
        const name = Name.create(raw.name).getOrThrow()
        const phoneNumber = PhoneNumber.create(raw.phoneNumber).getOrThrow()
        const vatNumber = VatNumber.create(raw.vatNumber).getOrThrow()
        const password = UserPassword.create({
            value: raw.password,
            hashed: true,
        }).getOrThrow()

        const client = Client.create(
            {
                email,
                name,
                phoneNumber,
                vatNumber,

                password,
            },
            new UniqueEntityID(raw.domainId),
        )

        if (client.isFailure) {
            return null
        }

        const cl = client.getValue()
        cl.status = raw.status as ClientStatus

        return cl
    }

    static toClientData(client: Client, tasks: IClientTaskDTO[]): IClientDataDTO {
        return {
            accountData: {
                name: client.name.value,
                email: client.email.value,
                phoneNumber: client.phoneNumber.value,
                vatNumber: client.vatNumber.value.toString(),
                status: client.status,
            },
            requestedTasks: {
                tasks: [...tasks],
            },
        }
    }
}
