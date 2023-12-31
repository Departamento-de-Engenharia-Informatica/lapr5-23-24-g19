import 'reflect-metadata'

import { expect } from 'chai'
import { describe, it } from 'mocha'
import { createSandbox } from 'sinon'
import Container from 'typedi'
import { Result } from '../core/logic/Result'
import Client from '../domain/user/client/Client'
import { VatNumber } from '../domain/user/client/vatNumber'
import { Email } from '../domain/user/email'
import { Name } from '../domain/user/name'
import { PhoneNumber } from '../domain/user/phoneNumber'
import { UserPassword } from '../domain/user/userPassword'
import { IClientDTO } from '../dto/IClientDTO'
import ClientService from './clientService'
import IAuthRepo from './IRepos/IAuthRepo'
import IClientRepo from './IRepos/IClientRepo'
import IMdtAdapter from './IRepos/IMdtRepo'
import { ICreatedClientDTO } from '../dto/ICreatedClientDTO'
import { ClientMap } from '../mappers/ClientMap'
import { IClientEmailDTO } from '../dto/IClientEmailDTO'
import { IClientDataRequestDTO } from '../dto/IClientDataRequestDTO'
import { ClientErrorCode, ClientErrorResult } from './IServices/IClientService'
import { IClientTaskDTO } from '../dto/IClientTaskDTO'
import { IClientDataDTO } from '../dto/IClientDataDTO'

describe('Client Service: Unit tests', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    beforeEach(() => {
        Container.reset()

        const clientSchema = require('../persistence/schemas/clientSchema').default
        Container.set('clientSchema', clientSchema)

        const clientRepoClass = require('../repos/mongo/clientRepo').default
        const clientRepo = Container.get(clientRepoClass)
        Container.set('ClientRepo', clientRepo)

        const authRepoClass = require('../repos/auth0/authRepo').default
        const authRepo = Container.get(authRepoClass)
        Container.set('AuthRepo', authRepo)

        const mdtAdapterClass = require('../repos/mdt/httpNodeMdtAdapter').default
        const mdtAdapter = Container.get(mdtAdapterClass)
        Container.set('HttpMdtAdapter', mdtAdapter)

        stubCreate(Client)
        stubCreate(Email)
        stubCreate(Name)
        stubCreate(PhoneNumber)
        stubCreate(VatNumber)
        stubCreate(UserPassword)
    })

    afterEach(() => sinon.restore())

    describe('createClient()', () => {
        it('should fail if client exists', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            sinon.stub(clientRepo, 'existsWithEmail').resolves(true)
            sinon.stub(clientRepo, 'save').rejects()

            const dto: IClientDTO = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912201029',
                vatNumber: 110220499,
                password: 'Password1$',
            }

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.createClient(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should succeed with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            sinon.stub(clientRepo, 'existsWithEmail').resolves(false)
            sinon.stub(clientRepo, 'save').resolves({} as unknown as Client)

            sinon.stub(ClientMap, 'toDTO').returns({} as unknown as ICreatedClientDTO)

            sinon.stub(authRepo, 'createUser').resolves()
            sinon.stub(authRepo, 'assignRoleToUser').resolves()
            sinon.stub(authRepo, 'blockUser').resolves()

            const dto: IClientDTO = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912201029',
                vatNumber: 110220499,
                password: 'Password1$',
            }

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.createClient(dto)

            expect(result.isRight()).to.be.true
        })
    })

    describe('getClientByState()', () => {
        it('should fail if clients do not exist', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            sinon.stub(clientRepo, 'findByState').resolves(undefined)

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.getClientsByState('Approved')

            expect(result.isLeft()).to.be.true
        })

        it('should succeed if clients exist', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const email = 'jjp@isep.ipp.pt'
            const name = 'João'
            const phoneNumber = '912201029'
            const vatNumber = 110220499

            sinon.stub(clientRepo, 'findByState').resolves([
                {
                    email: email as any,
                    name: name as any,
                    phoneNumber: phoneNumber as any,
                    vatNumber: vatNumber as any,
                },
            ] as Client[])

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.getClientsByState('Approved')

            expect(result.isRight()).to.be.true
        })
    })

    describe('patchClient()', () => {
        it('should fail if client does not exist', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            sinon.stub(clientRepo, 'find').resolves(undefined)

            const dto: IClientDTO = {
                email: 'hhf@isep.ipp.pt',
                name: 'Hugo',
                phoneNumber: '912201029',
                vatNumber: 110220499,
            } as IClientDTO

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.patchClient(dto)

            expect(result.isLeft()).to.be.true
        })

        // it('should fail if phone number is invalid', async () => {
        //     const clientRepo = Container.get('ClientRepo') as IClientRepo
        //     const authRepo = Container.get('AuthRepo') as IAuthRepo
        //     const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter
        //
        //     const email = 'nvm@isep.ipp.pt'
        //     const name = 'Nuno'
        //     const phoneNumber = '213912201029'
        //     const vatNumber = 110220499
        //
        //     sinon.stub(clientRepo, 'find').resolves({
        //         email: email as any,
        //         name: name as any,
        //         phoneNumber: '123123123' as any,
        //         vatNumber: vatNumber as any,
        //     } as Client)
        //
        //     const dto: IClientDTO = {
        //         email: email as any,
        //         name: name as any,
        //         phoneNumber: phoneNumber,
        //         vatNumber: vatNumber as any,
        //     } as IClientDTO
        //
        //     const service = new ClientService(clientRepo, authRepo, mdtAdapter)
        //     const result = await service.patchClient(dto)
        //
        //     expect(result.isLeft()).to.be.true
        // })
        //
        // it('should fail if vat number is invalid', async () => {
        //     const clientRepo = Container.get('ClientRepo') as IClientRepo
        //     const authRepo = Container.get('AuthRepo') as IAuthRepo
        //     const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter
        //
        //     const email = 'nvm@isep.ipp.pt'
        //     const name = 'Nuno'
        //     const phoneNumber = '997123123'
        //     const vatNumber = 11022049313129
        //
        //     sinon.stub(clientRepo, 'find').resolves({
        //         email: email as any,
        //         name: name as any,
        //         phoneNumber: phoneNumber as any,
        //         vatNumber: '123123123' as any,
        //     } as Client)
        //
        //     const dto: IClientDTO = {
        //         email: email as any,
        //         name: name as any,
        //         phoneNumber: phoneNumber as any,
        //         vatNumber: vatNumber as any,
        //     } as IClientDTO
        //
        //     const service = new ClientService(clientRepo, authRepo, mdtAdapter)
        //     const result = await service.patchClient(dto)
        //
        //     expect(result.isLeft()).to.be.true
        // })

        it('should succeed with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const email = 'llf@isep.ipp.pt'
            const name = 'Luis'
            const phoneNumber = '912201029'
            const vatNumber = 110220499

            sinon.stub(clientRepo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            sinon.stub(clientRepo, 'save').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            const dto: IClientDTO = {
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as IClientDTO

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.patchClient(dto)

            expect(result.isRight()).to.be.true
        })
    })

    describe('deleteClient()', () => {
        it('should succeed with right parameters', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            sinon.stub(clientRepo, 'find').resolves(undefined)
            sinon.stub(authRepo, 'deleteUser').resolves()
            sinon.stub(clientRepo, 'delete').resolves(true)

            const dto: IClientEmailDTO = {
                email: 'hhf@isep.ipp.pt',
            } as IClientEmailDTO

            const service = new ClientService(clientRepo, authRepo, mdtAdapter)
            const result = await service.deleteClient(dto)

            expect(result.isLeft()).to.be.true
        })
    })

    describe('getClientData()', () => {
        it('should fail if client does not exist', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const dto: IClientDataRequestDTO = {
                email: 'mzc@isep.ipp.pt',
            }

            sinon.stub(clientRepo, 'find').resolves(null)

            const svc = new ClientService(clientRepo, authRepo, mdtAdapter)

            const result = await svc.getClientData(dto)

            expect(result.isLeft()).to.be.true
            const err = result.value as ClientErrorResult
            expect(err.errorCode).to.equal(ClientErrorCode.NotFound)
        })

        it('should fail on mdt adapter error', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const dto: IClientDataRequestDTO = {
                email: 'mzc@isep.ipp.pt',
            }

            sinon
                .stub(clientRepo, 'find')
                .resolves({ email: dto.email } as unknown as Client)

            const errMsg = 'HTTP Request failed: Internal Server error'
            sinon.stub(mdtAdapter, 'getClientRequestedTasks').rejects({ message: errMsg })

            const svc = new ClientService(clientRepo, authRepo, mdtAdapter)

            const result = await svc.getClientData(dto)

            expect(result.isLeft()).to.be.true
            const err = result.value as ClientErrorResult
            expect(err.errorCode).to.equal(ClientErrorCode.BussinessRuleViolation)
            expect(err.message).to.equal(errMsg)
        })
        it('should succeed if client exist & mdt returns good response', async () => {
            const clientRepo = Container.get('ClientRepo') as IClientRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo
            const mdtAdapter = Container.get('HttpMdtAdapter') as IMdtAdapter

            const dto: IClientDataRequestDTO = {
                email: 'mzc@isep.ipp.pt',
            }

            const client = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria Zulmira',
                phoneNumber: '919355120',
                vatNumber: '211492012',
                status: 'Approved',
            }

            sinon.stub(clientRepo, 'find').resolves(client as unknown as Client)

            const mdtResponse: IClientTaskDTO[] = [
                // {{{ mdt response data
                {
                    surveillanceContact: {
                        name: 'zuzu',
                        phoneNumber: 919191333,
                    },
                    location: {
                        startingPoint: {
                            buildingCode: 'A',
                            floorNumber: 1,
                            x: 4,
                            y: 14,
                        },
                        endingPoint: {
                            buildingCode: 'A',
                            floorNumber: 1,
                            x: 4,
                            y: 14,
                        },
                    },
                    status: 0,
                    jobType: 0,
                },
                // }}}
            ]

            sinon.stub(mdtAdapter, 'getClientRequestedTasks').resolves()

            const clientData = {
                accountData: { ...client },
                requestedTasks: {
                    tasks: [...mdtResponse],
                },
            }
            sinon.stub(ClientMap, 'toClientData').returns(clientData)

            const svc = new ClientService(clientRepo, authRepo, mdtAdapter)

            const result = await svc.getClientData(dto)

            expect(result.isRight()).to.be.true
            const res = result.value as IClientDataDTO
            expect(res).to.deep.equal(clientData)
        })
    })
})
