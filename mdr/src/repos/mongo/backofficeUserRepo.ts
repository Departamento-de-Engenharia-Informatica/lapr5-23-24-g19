import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import config from '../../../config'

import { Email } from '../../domain/user/email'
import IBackofficeUserRepo from '../../services/IRepos/IBackofficeUserRepo'
import BackofficeUser from '../../domain/user/backofficeUser/backofficeUser'
import { IBackofficeUserPersistence } from '../../dataschema/mongo/IBackofficeUserPersistence'
import { BackofficeUserMap } from '../../mappers/BackofficeUserMap'

@Service()
export default class BackofficeUserRepo implements IBackofficeUserRepo {
    constructor(
        @Inject(config.schemas.backofficeUser.name)
        private schema: Model<IBackofficeUserPersistence & Document>,
    ) {}

    public async save(backofficeUser: BackofficeUser): Promise<BackofficeUser> {
        const doc = await this.schema.findOne({ email: backofficeUser.email.value })

        try {
            const raw = BackofficeUserMap.toPersistence(backofficeUser)

            if (!doc) {
                const created = await this.schema.create(raw)
                return BackofficeUserMap.toDomain(created)
            } else {
                doc.name = raw.name
                doc.role = raw.role
                doc.phoneNumber = raw.phoneNumber

                doc.password = raw.password

                await doc.save()
                return BackofficeUserMap.toDomain(doc)
            }
        } catch (err) {
            throw err
        }
    }

    async find(id: Email): Promise<BackofficeUser> {
        const doc = await this.schema.findOne({ email: id.value })

        if (!doc) {
            return null
        }

        return BackofficeUserMap.toDomain(doc)
    }

    delete(user: BackofficeUser): Promise<BackofficeUser> {
        throw new Error('Method not implemented.')
    }

    async exists(t: BackofficeUser): Promise<boolean> {
        return !!(await this.find(t.email))
    }

    async existsWithEmail(t: Email): Promise<boolean> {
        return !!(await this.find(t))
    }
}
