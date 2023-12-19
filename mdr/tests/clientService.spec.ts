import 'reflect-metadata'

import { expect } from 'chai'
import { describe, it } from 'mocha'
import { createSandbox } from 'sinon'
import Container from 'typedi'
import Client from '../src/domain/user/client/Client'
import { IClientDTO } from '../src/dto/IClientDTO'
import { ICreatedClientDTO } from '../src/dto/ICreatedClientDTO'
import { ClientMap } from '../src/mappers/ClientMap'
import ClientService from '../src/services/clientService'
import IClientRepo from '../src/services/IRepos/IClientRepo'

describe('Client Service: Integration tests', () => {
    const sinon = createSandbox()

    // function stubCreate<K>(klass: K) {
    //     sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    // }

    beforeEach(() => {
        Container.reset()

        const clientSchema = require('../src/persistence/schemas/clientSchema').default
        Container.set('clientSchema', clientSchema)

        const clientRepoClass = require('../src/repos/mongo/clientRepo').default
        const clientRepo = Container.get(clientRepoClass)
        Container.set('ClientRepo', clientRepo)
    })

    afterEach(() => sinon.restore())

    describe('createClient(): service + domain tests', () => {

        it('should fail if client exists', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'existsWithEmail').resolves(true)
            sinon.stub(clientRepo, 'save').rejects()


            const dto: IClientDTO = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912201029',
                vatNumber: 110220499,
                password: 'Password1$',
            }

            const service = new ClientService(clientRepo)
            const result = await service.createClient(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should succeed with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'existsWithEmail').resolves(false)
            sinon.stub(clientRepo, 'save').resolves({} as unknown as Client)

            sinon.stub(ClientMap, 'toDTO').returns({} as unknown as ICreatedClientDTO)

            const dto: IClientDTO = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912201029',
                vatNumber: 110220499,
                password: 'Password1$',
            }

            const service = new ClientService(clientRepo)
            const result = await service.createClient(dto)

            expect(result.isRight()).to.be.true
        })

    })
})
