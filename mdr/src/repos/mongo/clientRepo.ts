import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import config from '../../../config'

import { IClientPersistence } from '../../dataschema/mongo/IClientPersistence'
import IClientRepo from '../../services/IRepos/IClientRepo'
import Client from '../../domain/user/client/Client'
import { Email } from '../../domain/user/email'
import { ClientMap } from '../../mappers/ClientMap'

@Service()
export default class ClientRepo implements IClientRepo {
    constructor(
        @Inject(config.schemas.client.name)
        private schema: Model<IClientPersistence & Document>,
    ) {}

    public async save(client: Client): Promise<Client> {
        const doc = await this.schema.findOne({ email: client.email.value })

        try {
            const raw = ClientMap.toPersistence(client)

            if (!doc) {
                const created = await this.schema.create(raw)
                return ClientMap.toDomain(created)
            } else {
                doc.name = raw.name
                doc.phoneNumber = raw.phoneNumber
                doc.vatNumber = raw.vatNumber

                doc.status = raw.status
                doc.password = raw.password

                await doc.save()
                return ClientMap.toDomain(doc)
            }
        } catch (err) {
            throw err
        }
    }

    async find(id: Email): Promise<Client> {
        const doc = await this.schema.findOne({ email: id.value })

        if (!doc) {
            return null
        }

        return ClientMap.toDomain(doc)
    }

    async findByState(state: string): Promise<Client[]> {
        const docs = await this.schema.find({ status: state })
        if (!docs || !docs.length) {
            return null
        }

        return docs.map((doc) => ClientMap.toDomain(doc))
    }

    async delete(user: Client): Promise<boolean> {
        const doc = await this.schema.deleteOne({ email: user.email.value })

        if (!doc) {
            return null
        }

        return true
    }

    async exists(t: Client): Promise<boolean> {
        return !!(await this.find(t.email))
    }

    async existsWithEmail(t: Email): Promise<boolean> {
        return !!(await this.find(t))
    }
}
