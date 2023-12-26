// import 'reflect-metadata'
//
// import { NextFunction, Request, Response } from 'express'
// import * as sinon from 'sinon'
// import { Container } from 'typedi'
// import { Name } from '../src/domain/user/name'
// import { PhoneNumber } from '../src/domain/user/phoneNumber'
// import { VatNumber } from '../src/domain/user/client/vatNumber'
// import { UserPassword } from '../src/domain/user/userPassword'
// import { Email } from '../src/domain/user/email'
// import { ClientMap } from '../src/mappers/ClientMap'
// import IClientRepo from '../src/services/IRepos/IClientRepo'
// import Client from '../src/domain/user/client/Client'
// import IClientService from '../src/services/IServices/IClientService'
// import ClientController from '../src/controllers/clientController'
//
// describe('Client controller:', () => {
//     const sandbox = sinon.createSandbox()
//
//     beforeEach(function() {
//         Container.reset()
//
//         const schema = require('../src/persistence/schemas/clientSchema').default
//         Container.set('clientSchema', schema)
//
//         const repoClass = require('../src/repos/mongo/clientRepo').default
//         const repo = Container.get(repoClass)
//         Container.set('ClientRepo', repo)
//
//         const serviceClass = require('../src/services/clientService').default
//         const service = Container.get(serviceClass)
//         Container.set('ClientService', service)
//     })
//
//     afterEach(function() {
//         sandbox.restore()
//     })
//
//     describe('createClient(): clientController + clientService integration test using spy on clientService', () => {
//         it('should work with correct values', async () => {
//             const body = {
//                 email: Email.create('1181478@isep.ipp.pt').getValue(),
//                 name: Name.create('jonas').getValue(),
//                 phoneNumber: PhoneNumber.create(123456789).getValue(),
//                 vatNumber: VatNumber.create(123456789).getValue(),
//                 password: UserPassword.create({
//                     value: 'Ab1doistresquatro',
//                     hashed: true,
//                 }).getValue(),
//             }
//
//             const req: Partial<Request> = {}
//             req.body = body
//
//             const res: Partial<Response> = {
//                 status: sandbox.spy(),
//             }
//
//             const next: Partial<NextFunction> = () => {}
//
//             sandbox.stub(ClientMap, 'toDTO').returns({
//                 email: req.body.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//                 status: 'Pending',
//             })
//
//             const repo = Container.get('ClientRepo') as IClientRepo
//             sandbox.stub(repo, 'exists').resolves(false)
//             sandbox.stub(repo, 'save').resolves({
//                 email: req.body.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//             } as Client)
//
//             const service = Container.get('ClientService') as IClientService
//             const serviceSpy = sinon.spy(service, 'createClient')
//
//             const ctrl = new ClientController(service)
//             await ctrl.createClient(<Request>req, <Response>res, <NextFunction>next)
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(
//                 serviceSpy,
//                 sandbox.match({
//                     email: req.body.email,
//                     name: req.body.name,
//                     phoneNumber: req.body.phoneNumber,
//                     vatNumber: req.body.vatNumber,
//                 }),
//             )
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
//         })
//
//         it('should not work with invalid password', async () => {
//             const body = {
//                 email: Email.create('1181478@isep.ipp.pt').getValue(),
//                 name: Name.create('jonas').getValue(),
//                 phoneNumber: PhoneNumber.create(123456789).getValue(),
//                 vatNumber: VatNumber.create(123456789).getValue(),
//                 password: UserPassword.create({
//                     value: '',
//                     hashed: true,
//                 }).getValue(),
//             }
//
//             const req: Partial<Request> = {}
//             req.body = body
//
//             const res: Partial<Response> = {
//                 status: sandbox.spy(),
//             }
//
//             const next: Partial<NextFunction> = () => {}
//
//             sandbox.stub(ClientMap, 'toDTO').returns({
//                 email: req.body.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//                 status: 'Pending',
//             })
//
//             const repo = Container.get('ClientRepo') as IClientRepo
//             sandbox.stub(repo, 'exists').resolves(false)
//             sandbox.stub(repo, 'save').resolves({
//                 email: req.body.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//             } as Client)
//
//             const service = Container.get('ClientService') as IClientService
//             const serviceSpy = sinon.spy(service, 'createClient')
//
//             const ctrl = new ClientController(service)
//             await ctrl.createClient(<Request>req, <Response>res, <NextFunction>next)
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(
//                 serviceSpy,
//                 sandbox.match({
//                     email: req.body.email,
//                     name: req.body.name,
//                     phoneNumber: req.body.phoneNumber,
//                     vatNumber: req.body.vatNumber,
//                 }),
//             )
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
//         })
//
//         it('should not work with invalid phoneNumber value', async () => {
//             const body = {
//                 email: Email.create('1181478@isep.ipp.pt').getValue(),
//                 name: Name.create('jonas').getValue(),
//                 phoneNumber: undefined,
//                 vatNumber: VatNumber.create(123456789).getValue(),
//                 password: UserPassword.create({
//                     value: 'Ab1doistresquatro',
//                     hashed: true,
//                 }).getValue(),
//             }
//
//             const req: Partial<Request> = {}
//             req.body = body
//
//             const res: Partial<Response> = {
//                 status: sandbox.spy(),
//             }
//
//             const next: Partial<NextFunction> = () => {}
//
//             sandbox.stub(ClientMap, 'toDTO').returns({
//                 email: req.body.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//                 status: 'Pending',
//             })
//
//             const repo = Container.get('ClientRepo') as IClientRepo
//             sandbox.stub(repo, 'exists').resolves(false)
//             sandbox.stub(repo, 'save').resolves({
//                 email: req.body.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//             } as Client)
//
//             const service = Container.get('ClientService') as IClientService
//             const serviceSpy = sinon.spy(service, 'createClient')
//
//             const ctrl = new ClientController(service)
//             await ctrl.createClient(<Request>req, <Response>res, <NextFunction>next)
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(
//                 serviceSpy,
//                 sandbox.match({
//                     email: req.body.email,
//                     name: req.body.name,
//                     phoneNumber: req.body.phoneNumber,
//                     vatNumber: req.body.vatNumber,
//                 }),
//             )
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
//         })
//     })
//
//     describe('getClient(): clientController + clientService integration test using spy on clientService', () => {
//         it('should work with correct values', async () => {
//             const params = {
//                 email: Email.create('1211155@isep.ipp.pt').getValue().value,
//             }
//
//             const req: Partial<Request> = {}
//             req.params = params
//
//             const res: Partial<Response> = {
//                 status: sandbox.spy(),
//             }
//
//             const next: Partial<NextFunction> = () => {}
//
//             sandbox.stub(ClientMap, 'toDTO').returns({
//                 email: req.params.email,
//                 name: 'Diogo',
//                 phoneNumber: '996123123',
//                 vatNumber: 123456789,
//                 status: 'Pending',
//             })
//
//             const repo = Container.get('ClientRepo') as IClientRepo
//             sandbox.stub(repo, 'find').resolves({
//                 email: Email.create(params.email).getValue(),
//                 name: Name.create('Diogo').getValue(),
//                 phoneNumber: PhoneNumber.create(996123123).getValue(),
//                 vatNumber: VatNumber.create(123456789).getValue(),
//             } as Client)
//
//             const service = Container.get('ClientService') as IClientService
//             const serviceSpy = sinon.spy(service, 'getClient')
//
//             const ctrl = new ClientController(service)
//             await ctrl.getClient(<Request>req, <Response>res, <NextFunction>next)
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(
//                 serviceSpy,
//                 sandbox.match(req.params?.email as string),
//             )
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(200))
//         })
//     })
//
//     describe('patchClient(): clientController + clientService integration test using spy on clientService', () => {
//         it('should work with correct values', async () => {
//             const email = Email.create('1211155@isep.ipp.pt').getValue()
//             const body = {
//                 name: Name.create('Diogo').getValue().value,
//                 phoneNumber: PhoneNumber.create(123456789).getValue().value,
//                 vatNumber: VatNumber.create(123456789).getValue().value,
//             }
//
//             const req: Partial<Request> = {}
//             req.body = body
//             req.params = {
//                 email: email.value,
//             }
//
//             const res: Partial<Response> = {
//                 status: sandbox.spy(),
//             }
//
//             const next: Partial<NextFunction> = () => {}
//
//             sandbox.stub(ClientMap, 'toDTO').returns({
//                 email: req.params.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//                 status: 'Pending',
//             })
//
//             const repo = Container.get('ClientRepo') as IClientRepo
//
//             sandbox.stub(repo, 'find').resolves({
//                 email: email,
//                 name: Name.create('Marco').getValue(),
//                 phoneNumber: PhoneNumber.create(999999999).getValue(),
//                 vatNumber: VatNumber.create(123456123).getValue(),
//             } as Client)
//
//             sandbox.stub(repo, 'save').resolves({
//                 email: email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//             } as Client)
//
//             const service = Container.get('ClientService') as IClientService
//             const serviceSpy = sinon.spy(service, 'patchClient')
//
//             const ctrl = new ClientController(service)
//             await ctrl.patchClient(<Request>req, <Response>res, <NextFunction>next)
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(
//                 serviceSpy,
//                 sandbox.match({
//                     email: req.body.email,
//                     name: req.body.name,
//                     phoneNumber: req.body.phoneNumber,
//                     vatNumber: req.body.vatNumber,
//                 }),
//             )
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(200))
//         })
//
//         it('should not work with invalid phone number', async () => {
//             const email = Email.create('1211155@isep.ipp.pt').getValue()
//             const body = {
//                 name: Name.create('Diogo').getValue().value,
//                 phoneNumber: undefined,
//                 vatNumber: VatNumber.create(123456789).getValue().value,
//             }
//
//             const req: Partial<Request> = {}
//             req.body = body
//             req.params = {
//                 email: email.value,
//             }
//
//             const res: Partial<Response> = {
//                 status: sandbox.spy(),
//             }
//
//             const next: Partial<NextFunction> = () => {}
//
//             sandbox.stub(ClientMap, 'toDTO').returns({
//                 email: req.params.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//                 status: 'Pending',
//             })
//
//             const repo = Container.get('ClientRepo') as IClientRepo
//
//             sandbox.stub(repo, 'find').resolves({
//                 email: email,
//                 name: Name.create('Marco').getValue(),
//                 phoneNumber: PhoneNumber.create(999999999).getValue(),
//                 vatNumber: VatNumber.create(123456123).getValue(),
//             } as Client)
//
//             const service = Container.get('ClientService') as IClientService
//             const serviceSpy = sinon.spy(service, 'patchClient')
//
//             const ctrl = new ClientController(service)
//             await ctrl.patchClient(<Request>req, <Response>res, <NextFunction>next)
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(
//                 serviceSpy,
//                 sandbox.match({
//                     email: req.body.email,
//                     name: req.body.name,
//                     phoneNumber: req.body.phoneNumber,
//                     vatNumber: req.body.vatNumber,
//                 }),
//             )
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
//         })
//
//         it('should not work with invalid vat number', async () => {
//             const email = Email.create('1211155@isep.ipp.pt').getValue()
//             const body = {
//                 name: Name.create('Diogo').getValue().value,
//                 phoneNumber: PhoneNumber.create(123456789).getValue().value,
//                 vatNumber: undefined,
//             }
//
//             const req: Partial<Request> = {}
//             req.body = body
//             req.params = {
//                 email: email.value,
//             }
//
//             const res: Partial<Response> = {
//                 status: sandbox.spy(),
//             }
//
//             const next: Partial<NextFunction> = () => {}
//
//             sandbox.stub(ClientMap, 'toDTO').returns({
//                 email: req.params.email,
//                 name: req.body.name,
//                 phoneNumber: req.body.phoneNumber,
//                 vatNumber: req.body.vatNumber,
//                 status: 'Pending',
//             })
//
//             const repo = Container.get('ClientRepo') as IClientRepo
//
//             sandbox.stub(repo, 'find').resolves({
//                 email: email,
//                 name: Name.create('Marco').getValue(),
//                 phoneNumber: PhoneNumber.create(999999999).getValue(),
//                 vatNumber: VatNumber.create(123456123).getValue(),
//             } as Client)
//
//             const service = Container.get('ClientService') as IClientService
//             const serviceSpy = sinon.spy(service, 'patchClient')
//
//             const ctrl = new ClientController(service)
//             await ctrl.patchClient(<Request>req, <Response>res, <NextFunction>next)
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(
//                 serviceSpy,
//                 sandbox.match({
//                     email: req.body.email,
//                     name: req.body.name,
//                     phoneNumber: req.body.phoneNumber,
//                     vatNumber: req.body.vatNumber,
//                 }),
//             )
//
//             sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
//             sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
//         })
//     })
// })
