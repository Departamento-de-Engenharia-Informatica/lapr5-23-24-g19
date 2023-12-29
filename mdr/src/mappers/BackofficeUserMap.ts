import { UniqueEntityID } from '../core/domain/UniqueEntityID'
import { Mapper } from '../core/infra/Mapper'

import { Email } from '../domain/user/email'
import { Name } from '../domain/user/name'
import { PhoneNumber } from '../domain/user/phoneNumber'
import { UserPassword } from '../domain/user/userPassword'
import { IBackofficeUserPersistence } from '../dataschema/mongo/IBackofficeUserPersistence'
import BackofficeUser from '../domain/user/backofficeUser/backofficeUser'
import { ICreatedBackofficeUserDTO } from '../dto/ICreatedBackofficeUserDTO'
import Container from 'typedi'
import config from '../../config'
import IRoleRepo from '../services/IRepos/IRoleRepo'

export class BackofficeUserMap extends Mapper<BackofficeUser> {
    static toDTO(backofficeUser: BackofficeUser): ICreatedBackofficeUserDTO {
        return {
            email: backofficeUser.email.value,
            name: backofficeUser.name.value,
            role: backofficeUser.role.name,
            phoneNumber: backofficeUser.phoneNumber.value,
        }
    }

    static toPersistence(backofficeUser: BackofficeUser): IBackofficeUserPersistence {
        return {
            domainId: backofficeUser.id.toString(),
            email: backofficeUser.email.value,
            role: backofficeUser.role.name,
            name: backofficeUser.name.value,
            phoneNumber: backofficeUser.phoneNumber.value,

            password: backofficeUser.props.password.value,
        }
    }

    static async toDomain(raw: IBackofficeUserPersistence): Promise<BackofficeUser> {
        const email = Email.create(raw.email).getOrThrow()
        const name = Name.create(raw.name).getOrThrow()
        const phoneNumber = PhoneNumber.create(raw.phoneNumber).getOrThrow()
        const password = UserPassword.create({
            value: raw.password,
            hashed: true,
        }).getOrThrow()

        const roleRepo = Container.get(config.repos.role.name) as IRoleRepo
        const role = await roleRepo.find(raw.role)

        const backofficeUser = BackofficeUser.create(
            {
                email,
                role,
                name,
                phoneNumber,

                password,
            },
            new UniqueEntityID(raw.domainId),
        )

        if (backofficeUser.isFailure) {
            return null
        }

        const cl = backofficeUser.getValue()
        //cl.status = raw.status as ClientStatus

        return cl
    }
}
