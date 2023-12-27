import { Mapper } from '../core/infra/Mapper'

import { Document, Model } from 'mongoose'
import { IRolePersistence } from '../dataschema/mongo/IRolePersistence'

import IRoleDTO from '../dto/ICreateRoleDTO'
import { Role } from '../domain/role'

import { UniqueEntityID } from '../core/domain/UniqueEntityID'

export class RoleMap extends Mapper<Role> {
    public static toDTO(role: Role): IRoleDTO {
        return {
            // id: role.id.toString(),
            name: role.name,
        } as IRoleDTO
    }

    public static toDomain(role: IRolePersistence): Role {
        const roleOrError = Role.create(
            {
                name: role.name,
                active: role.active,
            },
            new UniqueEntityID(role.domainId),
        )

        roleOrError.isFailure ? console.log(roleOrError.error) : ''

        return roleOrError.isSuccess ? roleOrError.getValue() : null
    }

    public static toPersistence(role: Role): any {
        return {
            domainId: role.id.toString(),
            name: role.name,
            active: role.active,
        }
    }
}
