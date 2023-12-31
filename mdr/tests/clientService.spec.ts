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
import { IClientEmailDTO } from '../src/dto/IClientEmailDTO'
import { IClientWithoutPasswordDTO } from '../src/dto/IClientWithoutPasswordDTO'
import { ClientStatus } from '../src/domain/user/client/status'
import IUpdateClientStateDTO from '../src/dto/IUpdateClientStateDTO'

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

            sinon.stub(ClientMap, 'toDTO').returns({} as unknown as ICreatedClientDTO)

            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.createClient(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should succeed with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'existsWithEmail').resolves(false)
            sinon.stub(clientRepo, 'save').resolves({} as unknown as Client)

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

            sinon.stub(ClientMap, 'toDTO').returns({} as unknown as ICreatedClientDTO)

            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.createClient(dto)

            expect(result.isRight()).to.be.true
        })
    })

    describe('getClient(): service + domain tests', () => {
        it('should work with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'find').resolves({} as unknown as Client)
            sinon.stub(ClientMap, 'toDTO').returns({} as unknown as ICreatedClientDTO)

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
            sinon.stub(clientRepo, 'find').resolves({} as unknown as Client)

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

    describe('patchClient(): service + domain tests', () => {
        it('should work with right parameters', async () => {
            const email = 'nvm@isep.ipp.pt'
            const name = 'Nuno'
            const phoneNumber = '992201029'
            const vatNumber = 110220499

            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            const dto: IClientWithoutPasswordDTO = {
                email: email,
                name: name,
                phoneNumber: phoneNumber,
                vatNumber: vatNumber,
            }

            sinon.stub(clientRepo, 'save').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.patchClient(dto)

            expect(result.isRight()).to.be.true
        })

        it('should fail if client does not exist', async () => {
            const email = 'pdf@isep.ipp.pt'
            const name = 'Pedro'
            const phoneNumber = '992201029'
            const vatNumber = 110220499

            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'find').resolves(undefined)

            const dto: IClientWithoutPasswordDTO = {
                email: email,
                name: name,
                phoneNumber: phoneNumber,
                vatNumber: vatNumber,
            }

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.patchClient(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should fail with invalid phone number', async () => {
            const email = 'jar@isep.ipp.pt'
            const name = 'João'
            const phoneNumber = '99220102941341'
            const vatNumber = 110220499

            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            const dto: IClientWithoutPasswordDTO = {
                email: email,
                name: name,
                phoneNumber: phoneNumber,
                vatNumber: vatNumber,
            }

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.patchClient(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should fail with invalid vat number', async () => {
            const email = 'jar@isep.ipp.pt'
            const name = 'João'
            const phoneNumber = '123123123'
            const vatNumber = 110220494449

            const clientRepo = Container.get('ClientRepo') as IClientRepo
            sinon.stub(clientRepo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            const dto: IClientWithoutPasswordDTO = {
                email: email,
                name: name,
                phoneNumber: phoneNumber,
                vatNumber: vatNumber,
            }

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.patchClient(dto)

            expect(result.isLeft()).to.be.true
        })
    })

    describe('getClientByState(): service + domain tests', () => {
        it('should work if clients exist', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo

            sinon.stub(clientRepo, 'findByState').resolves([
                {
                    email: 'mzc@isep.ipp.pt' as any,
                    name: 'Maria' as any,
                    phoneNumber: '997123123' as any,
                    vatNumber: 123123123 as any,
                },
            ] as Client[])

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.getClientsByState('approved')

            expect(result.isRight()).to.be.true
        })

        it('should fail if no clients exist', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo

            sinon.stub(clientRepo, 'findByState').resolves(undefined)

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.getClientsByState('approved')

            expect(result.isLeft()).to.be.true
        })
    })

    describe('updateClientState(): service + domain tests', () => {
        it('should be able to approve client', async () => {
            const email = '1211155@isep.ipp.pt'
            const name = 'Nuno'
            const phoneNumber = '992201029'
            const vatNumber = 110220499

            const clientRepo = Container.get('ClientRepo') as IClientRepo

            sinon.stub(clientRepo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: ClientStatus.PENDING,
            } as Client)

            const dto: IUpdateClientStateDTO = {
                email: email,
                state: 'approved',
            }

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            sinon.stub(authRepo, 'unblockUser').resolves()

            sinon.stub(clientRepo, 'save').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: ClientStatus.APPROVED,
            } as Client)

            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.updateClientState(dto)

            expect(result.isRight()).to.be.true
        })

        it('should be able to reject client', async () => {
            const email = '1211155@isep.ipp.pt'
            const name = 'Nuno'
            const phoneNumber = '992201029'
            const vatNumber = 110220499

            const clientRepo = Container.get('ClientRepo') as IClientRepo

            sinon.stub(clientRepo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: ClientStatus.PENDING,
            } as Client)

            const dto: IUpdateClientStateDTO = {
                email: email,
                state: 'rejected',
            }

            sinon.stub(clientRepo, 'save').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: ClientStatus.APPROVED,
            } as Client)

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.updateClientState(dto)

            expect(result.isRight()).to.be.true
        })

        it('should not work if email is not valid', async () => {
            const email = '1211155@gmail.com'

            const clientRepo = Container.get('ClientRepo') as IClientRepo

            const dto: IUpdateClientStateDTO = {
                email: email,
                state: 'approved',
            }

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.updateClientState(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should not work if client does not exist', async () => {
            const email = '1211155@isep.ipp.pt'

            const clientRepo = Container.get('ClientRepo') as IClientRepo

            sinon.stub(clientRepo, 'find').resolves(undefined)

            const dto: IUpdateClientStateDTO = {
                email: email,
                state: 'approved',
            }

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.updateClientState(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should not work if client is already approved', async () => {
            const email = 'jas@isep.ipp.pt'
            const name = 'João'
            const phoneNumber = '992201029'
            const vatNumber = 110220499

            const clientRepo = Container.get('ClientRepo') as IClientRepo

            const dto: IUpdateClientStateDTO = {
                email: email,
                state: 'approved',
            }

            sinon.stub(clientRepo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: ClientStatus.APPROVED,
            } as Client)

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.updateClientState(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should not work if client is already rejected', async () => {
            const email = 'jas@isep.ipp.pt'
            const name = 'João'
            const phoneNumber = '992201029'
            const vatNumber = 110220499

            const clientRepo = Container.get('ClientRepo') as IClientRepo

            const dto: IUpdateClientStateDTO = {
                email: email,
                state: 'approved',
            }

            sinon.stub(clientRepo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: ClientStatus.APPROVED,
            } as Client)

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.updateClientState(dto)

            expect(result.isLeft()).to.be.true
        })
    })
})
