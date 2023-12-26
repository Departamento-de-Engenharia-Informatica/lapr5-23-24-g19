// import 'reflect-metadata'
//
// import { expect } from 'chai'
// import { describe, it } from 'mocha'
// import { createSandbox } from 'sinon'
// import Container from 'typedi'
// import { Result } from '../core/logic/Result'
// import Client from '../domain/user/client/Client'
// import { VatNumber } from '../domain/user/client/vatNumber'
// import { Email } from '../domain/user/email'
// import { Name } from '../domain/user/name'
// import { PhoneNumber } from '../domain/user/phoneNumber'
// import { UserPassword } from '../domain/user/userPassword'
// import { IClientDTO } from '../dto/IClientDTO'
// import { ICreatedClientDTO } from '../dto/ICreatedClientDTO'
// import { ClientMap } from '../mappers/ClientMap'
// import ClientService from './clientService'
// import IClientRepo from './IRepos/IClientRepo'
//
// describe('Client Service: Unit tests', () => {
//     const sinon = createSandbox()
//
//     function stubCreate<K>(klass: K) {
//         sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
//     }
//
//     beforeEach(() => {
//         Container.reset()
//
//         const clientSchema = require('../persistence/schemas/clientSchema').default
//         Container.set('clientSchema', clientSchema)
//
//         const clientRepoClass = require('../repos/mongo/clientRepo').default
//         const clientRepo = Container.get(clientRepoClass)
//         Container.set('ClientRepo', clientRepo)
//
//         stubCreate(Client)
//         stubCreate(Email)
//         stubCreate(Name)
//         stubCreate(PhoneNumber)
//         stubCreate(VatNumber)
//
//         stubCreate(UserPassword)
//     })
//
//     afterEach(() => sinon.restore())
//
//     describe('createClient()', () => {
//         it('should fail if client exists', async () => {
//             const clientRepo = Container.get('ClientRepo') as IClientRepo
//             sinon.stub(clientRepo, 'existsWithEmail').resolves(true)
//             sinon.stub(clientRepo, 'save').rejects()
//
//             const dto: IClientDTO = {
//                 email: 'mzc@isep.ipp.pt',
//                 name: 'Maria',
//                 phoneNumber: '912201029',
//                 vatNumber: 110220499,
//                 password: 'Password1$',
//             }
//
//             const service = new ClientService(clientRepo)
//             const result = await service.createClient(dto)
//
//             expect(result.isLeft()).to.be.true
//         })
//
//         it('should succeed with right parameters', async () => {
//             const clientRepo = Container.get('ClientRepo') as IClientRepo
//             sinon.stub(clientRepo, 'existsWithEmail').resolves(false)
//             sinon.stub(clientRepo, 'save').resolves({} as unknown as Client)
//
//             sinon.stub(ClientMap, 'toDTO').returns({} as unknown as ICreatedClientDTO)
//
//             const dto: IClientDTO = {
//                 email: 'mzc@isep.ipp.pt',
//                 name: 'Maria',
//                 phoneNumber: '912201029',
//                 vatNumber: 110220499,
//                 password: 'Password1$',
//             }
//
//             const service = new ClientService(clientRepo)
//             const result = await service.createClient(dto)
//
//             expect(result.isRight()).to.be.true
//         })
//     })
// })
