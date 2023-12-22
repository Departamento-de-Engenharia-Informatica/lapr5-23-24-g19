import 'reflect-metadata'

import { NextFunction, Request, Response } from 'express'
import * as sinon from 'sinon'
import { Container } from 'typedi'
import { Name } from '../src/domain/user/name'
import { PhoneNumber } from '../src/domain/user/phoneNumber'
import { UserPassword } from '../src/domain/user/userPassword'
import { Email } from '../src/domain/user/email'
import { BackofficeUserMap } from '../src/mappers/BackofficeUserMap'
import IBackofficeUserRepo from '../src/services/IRepos/IBackofficeUserRepo'
import BackofficeUser from '../src/domain/user/backofficeUser/backofficeUser'
import IBackofficeUserService from '../src/services/IServices/IBackofficeUserService'
import BackofficeUserController from '../src/controllers/backofficeUserController'

describe('BackofficeUser controller:', () => {
    const sandbox = sinon.createSandbox()

    beforeEach(function() {
        Container.reset()

        const schema = require('../src/persistence/schemas/backofficeUserSchema').default
        Container.set('backofficeUserSchema', schema)

        const repoClass = require('../src/repos/mongo/backofficeUserRepo').default
        const repo = Container.get(repoClass)
        Container.set('BackofficeUserRepo', repo)

        const serviceClass = require('../src/services/backofficeUserService').default
        const service = Container.get(serviceClass)
        Container.set('BackofficeUserService', service)
    })

    afterEach(function() {
        sandbox.restore()
    })

    describe('createBackofficeUser(): backofficeUserController + backofficeUserService integration test using spy on backofficeUserService', () => {
        it('should work with correct values', async () => {
            const body = {
                email: Email.create('1181478@isep.ipp.pt').getValue(),
                name: Name.create('jonas').getValue(),
                phoneNumber: PhoneNumber.create(123456789).getValue(),
                password: UserPassword.create({
                    value: 'Ab1doistresquatro',
                    hashed: true,
                }).getValue(),
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(BackofficeUserMap, 'toDTO').returns({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
            })

            const repo = Container.get('BackofficeUserRepo') as IBackofficeUserRepo
            sandbox.stub(repo, 'exists').resolves(false)
            sandbox.stub(repo, 'save').resolves({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
            } as BackofficeUser)

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
                    name: req.body.name,
                    phoneNumber: req.body.phoneNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
        })

        it('should not work with invalid password', async () => {
            const body = {
                email: Email.create('1181478@isep.ipp.pt').getValue(),
                name: Name.create('jonas').getValue(),
                phoneNumber: PhoneNumber.create(123456789).getValue(),
                password: UserPassword.create({
                    value: '',
                    hashed: true,
                }).getValue(),
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(BackofficeUserMap, 'toDTO').returns({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
            })

            const repo = Container.get('BackofficeUserRepo') as IBackofficeUserRepo
            sandbox.stub(repo, 'exists').resolves(false)
            sandbox.stub(repo, 'save').resolves({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
            } as BackofficeUser)

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
                    name: req.body.name,
                    phoneNumber: req.body.phoneNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
        })

        it('should not work with invalid phoneNumber value', async () => {
            const body = {
                email: Email.create('1181478@isep.ipp.pt').getValue(),
                name: Name.create('jonas').getValue(),
                phoneNumber: undefined,
                password: UserPassword.create({
                    value: 'Ab1doistresquatro',
                    hashed: true,
                }).getValue(),
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(BackofficeUserMap, 'toDTO').returns({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
            })

            const repo = Container.get('BackofficeUserRepo') as IBackofficeUserRepo
            sandbox.stub(repo, 'exists').resolves(false)
            sandbox.stub(repo, 'save').resolves({
                email: req.body.email,
                name: req.body.name,
                phoneNumber: req.body.phoneNumber,
            } as BackofficeUser)

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
                    name: req.body.name,
                    phoneNumber: req.body.phoneNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
        })
    })
})
