import 'reflect-metadata'

import { NextFunction, Request, Response } from 'express'
import * as sinon from 'sinon'
import { Container } from 'typedi'
import IAuthRepo from '../src/services/IRepos/IAuthRepo'
import { Result } from '../src/core/logic/Result'
import { Email } from '../src/domain/user/email'
import { UserPassword } from '../src/domain/user/userPassword'
import { PhoneNumber } from '../src/domain/user/phoneNumber'
import { Name } from '../src/domain/user/name'
import IClientRepo from '../src/services/IRepos/IClientRepo'
import { VatNumber } from '../src/domain/user/client/vatNumber'
import Client from '../src/domain/user/client/Client'
import { ClientMap } from '../src/mappers/ClientMap'
import IClientService from '../src/services/IServices/IClientService'
import ClientController from '../src/controllers/clientController'
import IArchiveService from '../src/services/IServices/IArchiveService'
import {IClientEmailDTO} from "../src/dto/IClientEmailDTO";

describe('Client controller:', () => {
    const sandbox = sinon.createSandbox()
    function stubCreate<K>(klass: K) {
        sandbox.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    beforeEach(function() {
        Container.reset()

        const schema = require('../src/persistence/schemas/clientSchema').default
        Container.set('clientSchema', schema)

        const repoClass = require('../src/repos/mongo/clientRepo').default
        const repo = Container.get(repoClass)
        Container.set('ClientRepo', repo)

        const authRepoClass = require('../src/repos/auth0/authRepo').default
        const authRepo = Container.get(authRepoClass)
        Container.set('AuthRepo', authRepo)

        const mdtAdapterClass = require('../src/repos/mdt/httpNodeMdtAdapter').default
        const mdtAdapter = Container.get(mdtAdapterClass)
        Container.set('HttpMdtAdapter', mdtAdapter)

        const serviceClass = require('../src/services/clientService').default
        const service = Container.get(serviceClass)
        Container.set('ClientService', service)

        const archiveServiceClass = require('../src/services/zipArchiveService').default
        const archiveService = Container.get(archiveServiceClass)
        Container.set('ZipArchiveService', archiveService)
    })

    afterEach(function() {
        sandbox.restore()
    })

    describe('createClient(): clientController + clientService integration test using spy on clientService', () => {
        it('should work with correct values', async () => {
            const body = {
                email: '1181478@isep.ipp.pt',
                name: 'jonas',
                phoneNumber: '123456789',
                vatNumber: '123455789',
                password: 'Jonasjonas123!',
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            stubCreate(Email)

            const repo = Container.get('ClientRepo') as IClientRepo
            sandbox.stub(repo, 'existsWithEmail').resolves(false)

            stubCreate(UserPassword)

            stubCreate(Name)
            stubCreate(PhoneNumber)
            stubCreate(VatNumber)
            stubCreate(Client)

            sandbox.stub(repo, 'save').resolves({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
                vatNumber: req.body.vatNumber,
            } as Client)

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            sandbox.stub(authRepo, 'createUser').resolves(
                JSON.stringify({
                    email: req.body.email,
                    password: req.body.password,
                    connection: 'Username-Password-Authentication',
                }),
            )

            sandbox.stub(authRepo, 'assignRoleToUser').resolves()

            sandbox.stub(authRepo, 'blockUser').resolves()

            sandbox.stub(ClientMap, 'toDTO').returns({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
                vatNumber: req.body.vatNumber,
                status: 'Pending',
            })

            const service = Container.get('ClientService') as IClientService
            const serviceSpy = sinon.spy(service, 'createClient')

            const archiveSvcSpy = Container.get('ZipArchiveService') as IArchiveService

            const ctrl = new ClientController(service, archiveSvcSpy)
            await ctrl.createClient(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    email: req.body.email,
                    name: req.body.name,
                    phoneNumber: req.body.phoneNumber,
                    vatNumber: req.body.vatNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(201))
        })
    })

    describe('deleteClient(): clientController + clientService integration test using spy on clientService', () => {
        it('should work with correct values', async () => {
            const email = '1155@isep.ipp.pt'
            const name = 'jonas'
            const phoneNumber = '910001029'
            const vatNumber = 110000499

            const params = {
                email: email,
            }

            const req: Partial<Request> = {}
            req.params = params

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            stubCreate(Email)

            const repo = Container.get('ClientRepo') as IClientRepo

            sandbox.stub(repo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            sandbox.stub(authRepo, 'deleteUser').resolves()

            sandbox.stub(repo, 'delete').resolves(true)

            const service = Container.get('ClientService') as IClientService
            const serviceSpy = sinon.spy(service, 'deleteClient')

            const archiveSvc = Container.get('ZipArchiveService') as IArchiveService

            const ctrl = new ClientController(service, archiveSvc)
            await ctrl.deleteClient(<Request>req, <Response>res, <NextFunction>next)

            /*sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(serviceSpy, sandbox.match(req.params.email))

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(200))*/
        })
    })

    describe('getClient(): clientController + clientService integration test using spy on clientService', () => {
        it('should work with correct email', async () => {
            const email = '1211155@isep.ipp.pt'
            const name = 'Maria'
            const phoneNumber = '912201029'
            const vatNumber = 110220499

            const params = {
                email: email,
            }

            const req: Partial<Request> = {}
            req.params = params

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            stubCreate(Email)

            const repo = Container.get('ClientRepo') as IClientRepo

            stubCreate(UserPassword)
            stubCreate(Name)
            stubCreate(PhoneNumber)
            stubCreate(VatNumber)
            stubCreate(Client)

            sandbox.stub(repo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            sandbox.stub(ClientMap, 'toDTO').returns({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: 'Pending',
            })

            const service = Container.get('ClientService') as IClientService
            const serviceSpy = sinon.spy(service, 'getClient')

            const archiveSvcSpy = Container.get('ZipArchiveService') as IArchiveService

            const ctrl = new ClientController(service, archiveSvcSpy)
            await ctrl.getClient(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(serviceSpy, sandbox.match(req.params.email))

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(200))
        })
    })

    describe('getClientsByState(): clientController + clientService integration test using spy on clientService', () => {
        it('should work with correct state', async () => {
            const state = 'Approved'
            const email = '1211155@isep.ipp.pt'
            const name = 'Maria'
            const phoneNumber = '912201029'
            const vatNumber = 110220499

            const query = {
                state: state,
            }

            const req: Partial<Request> = {}
            req.query = query

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            const repo = Container.get('ClientRepo') as IClientRepo

            stubCreate(UserPassword)
            stubCreate(Name)
            stubCreate(PhoneNumber)
            stubCreate(Client)

            sandbox.stub(repo, 'findByState').resolves([
                {
                    email: email as any,
                    name: name as any,
                    phoneNumber: phoneNumber as any,
                    vatNumber: vatNumber as any,
                } as Client,
            ])

            sandbox.stub(ClientMap, 'toDTO').returns({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: 'Pending',
            })

            const service = Container.get('ClientService') as IClientService
            const serviceSpy = sinon.spy(service, 'getClientsByState')

            const archiveSvcSpy = Container.get('ZipArchiveService') as IArchiveService

            const ctrl = new ClientController(service, archiveSvcSpy)
            await ctrl.getClientsByState(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(serviceSpy, sandbox.match(state))

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(200))
        })
    })

    describe('patchClient(): clientController + clientService integration test using spy on clientService', () => {
        it('should work if user exists', async () => {
            const email = '1211155@isep.ipp.pt'
            const name = 'Maria'
            const phoneNumber = '912201029'
            const vatNumber = 110220499

            const params = {
                email: email,
            }

            const req: Partial<Request> = {}
            req.params = params
            req.body = {
                email: email,
                name: name,
                phoneNumber: phoneNumber,
                vatNumber: vatNumber,
            }

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            stubCreate(Email)
            const repo = Container.get('ClientRepo') as IClientRepo

            stubCreate(UserPassword)
            stubCreate(Name)
            stubCreate(PhoneNumber)
            stubCreate(Client)

            sandbox.stub(repo, 'find').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            sandbox.stub(repo, 'save').resolves({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
            } as Client)

            sandbox.stub(ClientMap, 'toDTO').returns({
                email: email as any,
                name: name as any,
                phoneNumber: phoneNumber as any,
                vatNumber: vatNumber as any,
                status: 'Approved',
            })

            const service = Container.get('ClientService') as IClientService
            const serviceSpy = sinon.spy(service, 'patchClient')

            const archiveSvcSpy = Container.get('ZipArchiveService') as IArchiveService

            const ctrl = new ClientController(service, archiveSvcSpy)
            await ctrl.patchClient(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    email: req.body.email,
                    name: req.body.name,
                    phoneNumber: req.body.phoneNumber,
                    vatNumber: req.body.vatNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(200))
        })

        it('should not work if user does not exist', async () => {
            const email = '1211155@isep.ipp.pt'
            const name = 'Maria'
            const phoneNumber = '912201029'
            const vatNumber = 110220499

            const params = {
                email: email,
            }

            const req: Partial<Request> = {}
            req.params = params
            req.body = {
                email: email,
                name: name,
                phoneNumber: phoneNumber,
                vatNumber: vatNumber,
            }

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            stubCreate(Email)
            const repo = Container.get('ClientRepo') as IClientRepo

            sandbox.stub(repo, 'find').resolves(undefined)

            const service = Container.get('ClientService') as IClientService
            const serviceSpy = sinon.spy(service, 'patchClient')

            const archiveSvcSpy = Container.get('ZipArchiveService') as IArchiveService

            const ctrl = new ClientController(service, archiveSvcSpy)
            await ctrl.patchClient(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    email: req.body.email,
                    name: req.body.name,
                    phoneNumber: req.body.phoneNumber,
                    vatNumber: req.body.vatNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(404))
        })
    })
})
