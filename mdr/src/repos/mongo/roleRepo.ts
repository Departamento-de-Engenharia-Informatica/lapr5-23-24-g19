import { Service, Inject } from 'typedi'

import IRoleRepo from '../../services/IRepos/IRoleRepo'
import { Role } from '../../domain/role'
import { RoleId } from '../../domain/roleId'
import { RoleMap } from '../../mappers/RoleMap'

import { Document, FilterQuery, Model } from 'mongoose'
import { IRolePersistence } from '../../dataschema/mongo/IRolePersistence'

@Service()
export default class RoleRepo implements IRoleRepo {
    constructor(
        @Inject('roleSchema') private roleSchema: Model<IRolePersistence & Document>,
    ) {}

    public async exists(role: Role): Promise<boolean> {
        const idX = role.id instanceof RoleId ? (<RoleId>role.id).toValue() : role.id

        const query = { domainId: idX }
        const roleDocument = await this.roleSchema.findOne(
            query as FilterQuery<IRolePersistence & Document>,
        )

        return !!roleDocument === true
    }

    public async save(role: Role): Promise<Role> {
        const query = { domainId: role.id.toString() }

        const roleDocument = await this.roleSchema.findOne(query)

        try {
            if (roleDocument === null) {
                const rawRole: any = RoleMap.toPersistence(role)

                const roleCreated = await this.roleSchema.create(rawRole)

                return RoleMap.toDomain(roleCreated)
            } else {
                // roleDocument.name = role.name
                roleDocument.active = role.active
                await roleDocument.save()

                return role
            }
        } catch (err) {
            throw err
        }
    }

    public async findByDomainId(roleId: RoleId | string): Promise<Role> {
        const query = { domainId: roleId }
        const roleRecord = await this.roleSchema.findOne(
            query as FilterQuery<IRolePersistence & Document>,
        )

        if (roleRecord != null) {
            return RoleMap.toDomain(roleRecord)
        } else return null
    }

    async find(name: string): Promise<Role> {
        const doc = await this.roleSchema.findOne({ name })
        if (!doc) {
            return null
        }

        return RoleMap.toDomain(doc)
    }

    async activeRoles(): Promise<Role[]> {
        const doc = await this.roleSchema.find({ active: true })
        if (!doc) {
            return []
        }

        return doc.map((r) => RoleMap.toDomain(r))
    }
}
