import 'reflect-metadata'

import { expect } from 'chai'
import { describe, it } from 'mocha'
import { createSandbox } from 'sinon'
import Container from 'typedi'
import { Result } from '../core/logic/Result'
import { Email } from '../domain/user/email'
import { Name } from '../domain/user/name'
import { PhoneNumber } from '../domain/user/phoneNumber'
import { UserPassword } from '../domain/user/userPassword'
import BackofficeUser from '../domain/user/backofficeUser/backofficeUser'
import IBackofficeUserRepo from './IRepos/IBackofficeUserRepo'
import { IBackofficeUserDTO } from '../dto/IBackofficeUserDTO'
import BackofficeUserService from './backofficeUserService'
import { BackofficeUserMap } from '../mappers/BackofficeUserMap'
import { ICreatedBackofficeUserDTO } from '../dto/ICreatedBackofficeUserDTO'
import config from '../../config'
import IRoleRepo from './IRepos/IRoleRepo'
import { Role } from '../domain/role'
import IAuthRepo from './IRepos/IAuthRepo'
import Client from '../domain/user/client/Client'

describe('BackofficeUser Service: Unit tests', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    beforeEach(() => {
        Container.reset()

        const backofficeUserSchema =
            require('../persistence/schemas/backofficeUserSchema').default
        Container.set('backofficeUserSchema', backofficeUserSchema)

        const roleSchema = require('../persistence/schemas/roleSchema').default
        Container.set('roleSchema', roleSchema)

        const backofficeUserRepoClass =
            require('../repos/mongo/backofficeUserRepo').default
        const backofficeUserRepo = Container.get(backofficeUserRepoClass)
        Container.set('BackofficeUserRepo', backofficeUserRepo)

        const authRepoClass = require('../repos/auth0/authRepo').default
        const authRepo = Container.get(authRepoClass)
        Container.set('AuthRepo', authRepo)

        const roleRepoClass = require('../repos/mongo/roleRepo').default
        const roleRepo = Container.get(roleRepoClass)
        Container.set('RoleRepo', roleRepo)

        stubCreate(BackofficeUser)
        stubCreate(Email)
        stubCreate(Name)
        stubCreate(PhoneNumber)

        stubCreate(UserPassword)
    })

    afterEach(() => sinon.restore())

    describe('createBackofficeUser()', () => {
        it('should fail if backoffice user exists', async () => {
            const dto: IBackofficeUserDTO = {
                email: 'mzc@isep.ipp.pt',
                role: 'Campus Manager',
                name: 'Maria',
                phoneNumber: '912201029',
                password: 'Password1$',
            }

            const backofficeUserRepo = Container.get(
                'BackofficeUserRepo',
            ) as IBackofficeUserRepo
            sinon.stub(backofficeUserRepo, 'existsWithEmail').resolves(true)

            const roleRepo = Container.get(config.repos.role.name) as IRoleRepo
            sinon.stub(roleRepo, 'find').resolves(<Role>{ name: dto.role })

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            sinon.stub(authRepo, 'createUser').resolves(
                JSON.stringify({
                    email: dto.email,
                    password: dto.password,
                    connection: 'Username-Password-Authentication',
                }),
            )

            sinon.stub(authRepo, 'assignRoleToUser').resolves()
            sinon.stub(backofficeUserRepo, 'save').rejects()

            const service = new BackofficeUserService(
                backofficeUserRepo,
                authRepo,
                roleRepo,
            )
            const result = await service.createBackofficeUser(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should succeed with right parameters', async () => {
            const backofficeUserRepo = Container.get(
                'BackofficeUserRepo',
            ) as IBackofficeUserRepo
            const roleRepo = Container.get('RoleRepo') as IRoleRepo
            const authRepo = Container.get('AuthRepo') as IAuthRepo

            sinon.stub(backofficeUserRepo, 'existsWithEmail').resolves(false)
            sinon
                .stub(backofficeUserRepo, 'save')
                .rejects({} as unknown as BackofficeUser)

            sinon
                .stub(BackofficeUserMap, 'toDTO')
                .returns({} as unknown as ICreatedBackofficeUserDTO)

            sinon.stub(roleRepo, 'find').resolves()

            sinon.stub(authRepo, 'createUser').resolves()
            sinon.stub(authRepo, 'assignRoleToUser').resolves()

            const dto: IBackofficeUserDTO = {
                email: 'mzcx@isep.ipp.pt',
                role: 'Campus Manager',
                name: 'Maria',
                phoneNumber: '911201029',
                password: 'Password1$',
            }

            const service = new BackofficeUserService(
                backofficeUserRepo,
                authRepo,
                roleRepo,
            )
            const result = await service.createBackofficeUser(dto)

            expect(result.isLeft()).to.be.true
        })
    })
})
