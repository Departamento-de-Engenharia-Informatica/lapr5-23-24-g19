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
import IAuthRepo from '../src/services/IRepos/IAuthRepo'
import IMdtAdapter from '../src/services/IRepos/IMdtRepo'
import {IClientEmailDTO} from "../src/dto/IClientEmailDTO";

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

        const authRepoClass = require('../src/repos/auth0/authRepo').default
        const authRepo = Container.get(authRepoClass)
        Container.set('AuthRepo', authRepo)

        const mdtAdapterClass = require('../src/repos/mdt/httpNodeMdtAdapter').default
        const mdtAdapter = Container.get(mdtAdapterClass)
        Container.set('HttpMdtAdapter', mdtAdapter)
    })

    afterEach(() => sinon.restore())

    describe('createClient(): service + domain tests', () => {
        it('should fail if client exists', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'existsWithEmail').resolves(true)


            const dto: IClientDTO = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912201029',
                vatNumber: 110220499,
                password: 'Password1$',
            }

            sinon.stub(clientRepo, 'save').rejects()

            const authRepo = Container.get('AuthRepo') as IAuthRepo

            sinon.stub(authRepo, 'createUser').resolves(
                JSON.stringify({
                    email: dto.email,
                    password: dto.password,
                    connection: 'Username-Password-Authentication',
                }),
            )
            sinon.stub(authRepo, 'assignRoleToUser').resolves()
            sinon.stub(authRepo, 'blockUser').resolves()

            sinon.stub(ClientMap, 'toDTO').returns(({} as unknown) as ICreatedClientDTO)

            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.createClient(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should succeed with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'existsWithEmail').resolves(false)
            sinon.stub(clientRepo, 'save').resolves(({} as unknown) as Client)


            const dto: IClientDTO = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912201029',
                vatNumber: 110220499,
                password: 'Password1$',
            }


            const authRepo = Container.get('AuthRepo') as IAuthRepo

            sinon.stub(authRepo, 'createUser').resolves(
                JSON.stringify({
                    email: dto.email,
                    password: dto.password,
                    connection: 'Username-Password-Authentication',
                }),
            )
            sinon.stub(authRepo, 'assignRoleToUser').resolves()
            sinon.stub(authRepo, 'blockUser').resolves()

            sinon.stub(ClientMap, 'toDTO').returns(({} as unknown) as ICreatedClientDTO)


            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.createClient(dto)

            expect(result.isRight()).to.be.true
        })
    })

    describe('getClient(): service + domain tests', () => {
        it('should work with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'find').resolves(({} as unknown) as Client)
            sinon.stub(ClientMap, 'toDTO').returns(({} as unknown) as ICreatedClientDTO)


            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)


            const result = await service.getClient('1211155@isep.ipp.pt')

            expect(result.isRight()).to.be.true
        })
    })

   /* describe('getClientsByState(): service + domain tests', () => {
        it('should work with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'findByState').resolves(({} as unknown) as Client[])
            sinon.stub(ClientMap, 'toDTO').returns(({} as unknown) as ICreatedClientDTO)



            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)


            const result = await service.getClientsByState('Pending')


            expect(result.isRight()).to.be.true
        })
    })*/

    describe('deleteClient(): service + domain tests', () => {
        it('should work with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'find').resolves(({} as unknown) as Client)


            const dto: IClientEmailDTO = {
                email: 'mzc@isep.ipp.pt',
            }

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            sinon.stub(authRepo, 'deleteUser').resolves()


            sinon.stub(clientRepo, 'delete').resolves(true)

            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)


            const result = await service.deleteClient(dto)


            expect(result.isRight()).to.be.true
        })
    })
})
