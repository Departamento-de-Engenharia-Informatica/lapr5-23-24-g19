// import 'reflect-metadata'
//
// import { expect } from 'chai'
// import { describe, it } from 'mocha'
// import { createSandbox } from 'sinon'
// import Container from 'typedi'
// import { Result } from '../core/logic/Result'
// import { Email } from '../domain/user/email'
// import { Name } from '../domain/user/name'
// import { PhoneNumber } from '../domain/user/phoneNumber'
// import { UserPassword } from '../domain/user/userPassword'
// import BackofficeUser from '../domain/user/backofficeUser/backofficeUser'
// import IBackofficeUserRepo from './IRepos/IBackofficeUserRepo'
// import { IBackofficeUserDTO } from '../dto/IBackofficeUserDTO'
// import BackofficeUserService from './backofficeUserService'
// import { BackofficeUserMap } from '../mappers/BackofficeUserMap'
// import { ICreatedBackofficeUserDTO } from '../dto/ICreatedBackofficeUserDTO'
// import config from '../../config'
// import IRoleRepo from './IRepos/IRoleRepo'
// import { Role } from '../domain/role'
//
// describe('BackofficeUser Service: Unit tests', () => {
//     const sinon = createSandbox()
//
//     function stubCreate<K>(klass: K) {
//         sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
//     }
//
//     beforeEach(() => {
//         Container.reset()
//
//         const backofficeUserSchema = require('../persistence/schemas/backofficeUserSchema')
//             .default
//         Container.set('backofficeUserSchema', backofficeUserSchema)
//
//         const backofficeUserRepoClass = require('../repos/mongo/backofficeUserRepo')
//             .default
//         const backofficeUserRepo = Container.get(backofficeUserRepoClass)
//         Container.set('BackofficeUserRepo', backofficeUserRepo)
//
//         stubCreate(BackofficeUser)
//         stubCreate(Email)
//         stubCreate(Name)
//         stubCreate(PhoneNumber)
//
//         stubCreate(UserPassword)
//     })
//
//     afterEach(() => sinon.restore())
//
//     describe('createBackofficeUser()', () => {
//         it('should fail if backoffice user exists', async () => {
//             const backofficeUserRepo = Container.get(
//                 'BackofficeUserRepo',
//             ) as IBackofficeUserRepo
//             sinon.stub(backofficeUserRepo, 'existsWithEmail').resolves(true)
//             sinon.stub(backofficeUserRepo, 'save').rejects()
//
//             const dto: IBackofficeUserDTO = {
//                 email: 'mzc@isep.ipp.pt',
//                 role: 'Campus Manager',
//                 name: 'Maria',
//                 phoneNumber: '912201029',
//                 password: 'Password1$',
//             }
//
//             const roleRepo = Container.get(config.repos.role.name) as IRoleRepo
//             sinon.stub(roleRepo, 'find').resolves(<Role>{ name: dto.role })
//
//             const service = new BackofficeUserService(backofficeUserRepo, roleRepo)
//             const result = await service.createBackofficeUser(dto)
//
//             expect(result.isLeft()).to.be.true
//         })
//
//         it('should succeed with right parameters', async () => {
//             const backofficeUserRepo = Container.get(
//                 'BackofficeUserRepo',
//             ) as IBackofficeUserRepo
//             sinon.stub(backofficeUserRepo, 'existsWithEmail').resolves(false)
//             sinon
//                 .stub(backofficeUserRepo, 'save')
//                 .resolves(({} as unknown) as BackofficeUser)
//
//             sinon
//                 .stub(BackofficeUserMap, 'toDTO')
//                 .returns(({} as unknown) as ICreatedBackofficeUserDTO)
//
//             const dto: IBackofficeUserDTO = {
//                 email: 'mzc@isep.ipp.pt',
//                 role: 'Campus Manager',
//                 name: 'Maria',
//                 phoneNumber: '912201029',
//                 password: 'Password1$',
//             }
//
//             const roleRepo = Container.get(config.repos.role.name) as IRoleRepo
//             sinon.stub(roleRepo, 'find').resolves(<Role>{ name: dto.role })
//
//             const service = new BackofficeUserService(backofficeUserRepo, roleRepo)
//             const result = await service.createBackofficeUser(dto)
//
//             expect(result.isRight()).to.be.true
//         })
//     })
// })
