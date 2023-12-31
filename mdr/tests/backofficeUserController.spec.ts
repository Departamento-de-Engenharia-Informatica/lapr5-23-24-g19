import 'reflect-metadata'

import { NextFunction, Request, Response } from 'express'
import * as sinon from 'sinon'
import { Container } from 'typedi'
import { BackofficeUserMap } from '../src/mappers/BackofficeUserMap'
import IBackofficeUserRepo from '../src/services/IRepos/IBackofficeUserRepo'
import BackofficeUser from '../src/domain/user/backofficeUser/backofficeUser'
import IBackofficeUserService from '../src/services/IServices/IBackofficeUserService'
import BackofficeUserController from '../src/controllers/backofficeUserController'
import IAuthRepo from '../src/services/IRepos/IAuthRepo'
import { Result } from '../src/core/logic/Result'
import { Email } from '../src/domain/user/email'
import IRoleRepo from '../src/services/IRepos/IRoleRepo'
import { Role } from '../src/domain/role'
import { UserPassword } from '../src/domain/user/userPassword'
import { PhoneNumber } from '../src/domain/user/phoneNumber'
import { Name } from '../src/domain/user/name'

describe('BackofficeUser controller:', () => {
    const sandbox = sinon.createSandbox()
    function stubCreate<K>(klass: K) {
        sandbox.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    beforeEach(function () {
        Container.reset()

        const schema = require('../src/persistence/schemas/backofficeUserSchema').default
        Container.set('backofficeUserSchema', schema)

        const roleSchema = require('../src/persistence/schemas/roleSchema').default
        Container.set('roleSchema', roleSchema)

        const repoClass = require('../src/repos/mongo/backofficeUserRepo').default
        const repo = Container.get(repoClass)
        Container.set('BackofficeUserRepo', repo)

        const authRepoClass = require('../src/repos/auth0/authRepo').default
        const authRepo = Container.get(authRepoClass)
        Container.set('AuthRepo', authRepo)

        const roleRepoClass = require('../src/repos/mongo/roleRepo').default
        const roleRepo = Container.get(roleRepoClass)
        Container.set('RoleRepo', roleRepo)

        const serviceClass = require('../src/services/backofficeUserService').default
        const service = Container.get(serviceClass)
        Container.set('BackofficeUserService', service)
    })

    afterEach(function () {
        sandbox.restore()
    })

    describe('createBackofficeUser(): backofficeUserController + backofficeUserService integration test using spy on backofficeUserService', () => {
        it('should work with correct values', async () => {
            const body = {
                email: '1181478@isep.ipp.pt',
                role: 'Fleet Manager',
                name: 'jonas',
                phoneNumber: '123456789',
                password: 'Jonasjonas123!',
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            stubCreate(Email)

            const repo = Container.get('BackofficeUserRepo') as IBackofficeUserRepo
            sandbox.stub(repo, 'existsWithEmail').resolves(false)

            const roleRepo = Container.get('RoleRepo') as IRoleRepo
            sandbox.stub(roleRepo, 'find').resolves({
                name: 'Fleet Manager',
                active: true,
            } as Role)

            stubCreate(UserPassword)
            stubCreate(Name)
            stubCreate(PhoneNumber)
            stubCreate(BackofficeUser)

            sandbox.stub(repo, 'save').resolves({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
            } as BackofficeUser)

            const authRepo = Container.get('AuthRepo') as IAuthRepo
            sandbox.stub(authRepo, 'createUser').resolves(
                JSON.stringify({
                    email: req.body.email,
                    password: req.body.password,
                    connection: 'Username-Password-Authentication',
                }),
            )

            sandbox.stub(authRepo, 'assignRoleToUser').resolves()

            sandbox.stub(BackofficeUserMap, 'toDTO').returns({
                email: req.body.email,
                name: req.body.name,
                role: req.body.role,
                phoneNumber: req.body.phoneNumber,
            })

            const service = Container.get(
                'BackofficeUserService',
            ) as IBackofficeUserService
            const serviceSpy = sinon.spy(service, 'createBackofficeUser')

            const ctrl = new BackofficeUserController(service)
            await ctrl.createBackofficeUser(
                <Request>req,
                <Response>res,
                <NextFunction>next,
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    email: req.body.email,
                    role: req.body.role,
                    name: req.body.name,
                    phoneNumber: req.body.phoneNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(201))
        })
    })
})
