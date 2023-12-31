import 'reflect-metadata'

import { expect } from 'chai'
import { describe, it } from 'mocha'
import { createSandbox } from 'sinon'
import Container from 'typedi'
import IBackofficeUserRepo from '../src/services/IRepos/IBackofficeUserRepo'
import BackofficeUserService from '../src/services/backofficeUserService'
import { ICreatedBackofficeUserDTO } from '../src/dto/ICreatedBackofficeUserDTO'
import { BackofficeUserMap } from '../src/mappers/BackofficeUserMap'
import { IBackofficeUserDTO } from '../src/dto/IBackofficeUserDTO'
import BackofficeUser from '../src/domain/user/backofficeUser/backofficeUser'
import config from '../config'
import IRoleRepo from '../src/services/IRepos/IRoleRepo'
import { Role } from '../src/domain/role'
import IAuthRepo from '../src/services/IRepos/IAuthRepo'

describe('Backoffice user Service: Integration tests', () => {
    const sinon = createSandbox()

    // function stubCreate<K>(klass: K) {
    //     sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    // }

    beforeEach(() => {
        Container.reset()

        const backofficeUserSchema =
            require('../src/persistence/schemas/backofficeUserSchema').default
        Container.set('backofficeUserSchema', backofficeUserSchema)

        const roleSchema = require('../src/persistence/schemas/roleSchema').default
        Container.set('roleSchema', roleSchema)

        const backofficeUserRepoClass =
            require('../src/repos/mongo/backofficeUserRepo').default
        const backofficeUserRepo = Container.get(backofficeUserRepoClass)
        Container.set('backofficeUserRepo', backofficeUserRepo)

        const authRepoClass = require('../src/repos/auth0/authRepo').default
        const authRepo = Container.get(authRepoClass)
        Container.set('AuthRepo', authRepo)

        const roleRepoClass = require('../src/repos/mongo/roleRepo').default
        const roleRepo = Container.get(roleRepoClass)
        Container.set('RoleRepo', roleRepo)
    })

    afterEach(() => sinon.restore())

    describe('createBackofficeUser(): service + domain tests', () => {
        it('should fail if backoffice user exists', async () => {
            const backofficeUserRepo = Container.get(
                'BackofficeUserRepo',
            ) as IBackofficeUserRepo
            sinon.stub(backofficeUserRepo, 'existsWithEmail').resolves(true)

            const dto: IBackofficeUserDTO = {
                email: 'mzc@isep.ipp.pt',
                role: 'Task Manager',
                name: 'Maria',
                phoneNumber: '912201029',
                password: 'Password1$',
            }

            const roleRepo = Container.get('RoleRepo') as IRoleRepo
            sinon.stub(roleRepo, 'find').resolves(<Role>{ name: dto.role })

            sinon.stub(backofficeUserRepo, 'save').rejects()

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            sinon.stub(authRepo, 'createUser').resolves(
                JSON.stringify({
                    email: dto.email,
                    password: dto.password,
                    connection: 'Username-Password-Authentication',
                }),
            )

            sinon.stub(authRepo, 'assignRoleToUser').resolves()

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
            sinon.stub(backofficeUserRepo, 'existsWithEmail').resolves(false)
            sinon
                .stub(backofficeUserRepo, 'save')
                .resolves({} as unknown as BackofficeUser)

            sinon
                .stub(BackofficeUserMap, 'toDTO')
                .returns({} as unknown as ICreatedBackofficeUserDTO)

            const dto: IBackofficeUserDTO = {
                email: 'mzc@isep.ipp.pt',
                role: 'Task Manager',
                name: 'Maria',
                phoneNumber: '912201029',
                password: 'Password1$',
            }

            const roleRepo = Container.get('RoleRepo') as IRoleRepo
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

            const service = new BackofficeUserService(
                backofficeUserRepo,
                authRepo,
                roleRepo,
            )
            const result = await service.createBackofficeUser(dto)

            expect(result.isRight()).to.be.true
        })
    })
})
