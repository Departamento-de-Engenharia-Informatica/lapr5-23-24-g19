import 'reflect-metadata'

import { NextFunction, Request, Response } from 'express'
import * as sinon from 'sinon'
import { Container } from 'typedi'

import RobotTypeController from '../src/controllers/robotTypeController'
import RobotType from '../src/domain/robotType/robotType'
import { RobotTypeMap } from '../src/mappers/RobotTypeMap'
import IRobotTypeRepo from '../src/services/IRepos/IRobotTypeRepo'
import IRobotTypeService from '../src/services/IServices/IRobotTypeService'

describe('RobotType Controller', function () {
    const sandbox = sinon.createSandbox()

    beforeEach(function () {
        Container.reset()

        let schema = require('../src/persistence/schemas/robotTypeSchema').default
        Container.set('robotType', schema)

        let repoClass = require('../src/repos/mongo/robotTypeRepo').default
        let repo = Container.get(repoClass)
        Container.set('RobotTypeRepo', repo)

        let serviceClass = require('../src/services/robotTypeService').default
        let service = Container.get(serviceClass)
        Container.set('RobotTypeService', service)
    })

    afterEach(function () {
        sandbox.restore()
    })

    describe('createRobotType(): robotTypeController + robotTypeService integration test using spy on robotTypeService', function () {
        it('should work with correct values', async () => {
            const body = {
                code: 'RobotType1',
                brand: 'Apple',
                model: 'X11',
                taskTypes: ['DELIVERY'],
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotTypeMap, 'toDTO').returns({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskTypes: req.body.taskTypes,
            })

            let repo = Container.get('RobotTypeRepo') as IRobotTypeRepo
            sandbox.stub(repo, 'exists').resolves(false)
            sandbox.stub(repo, 'save').resolves({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskType: req.body.taskTypes,
            } as RobotType)

            const service = Container.get('RobotTypeService') as IRobotTypeService
            const serviceSpy = sinon.spy(service, 'createRobotType')

            const ctrl = new RobotTypeController(service)
            await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.body.code,
                    brand: req.body.brand,
                    model: req.body.model,
                    taskTypes: req.body.taskTypes,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(201))
        })

        it('should not work with invalid code', async () => {
            const body = {
                code: undefined,
                brand: 'Apple',
                model: 'X11',
                taskTypes: ['DELIVERY'],
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotTypeMap, 'toDTO').returns({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskTypes: req.body.taskTypes,
            })

            let repo = Container.get('RobotTypeRepo') as IRobotTypeRepo
            sandbox.stub(repo, 'exists').resolves(false)
            sandbox.stub(repo, 'save').resolves({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskType: req.body.taskTypes,
            } as RobotType)

            const service = Container.get('RobotTypeService') as IRobotTypeService
            const serviceSpy = sinon.spy(service, 'createRobotType')

            const ctrl = new RobotTypeController(service)
            await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.body.code,
                    brand: req.body.brand,
                    model: req.body.model,
                    taskTypes: req.body.taskTypes,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
        })

        it('should not work with invalid brand', async () => {
            const body = {
                code: '%%%%%%',
                brand: undefined,
                model: 'X11',
                taskTypes: ['DELIVERY'],
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotTypeMap, 'toDTO').returns({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskTypes: req.body.taskTypes,
            })

            let repo = Container.get('RobotTypeRepo') as IRobotTypeRepo
            sandbox.stub(repo, 'exists').resolves(false)
            sandbox.stub(repo, 'save').resolves({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskType: req.body.taskTypes,
            } as RobotType)

            const service = Container.get('RobotTypeService') as IRobotTypeService
            const serviceSpy = sinon.spy(service, 'createRobotType')

            const ctrl = new RobotTypeController(service)
            await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.body.code,
                    brand: req.body.brand,
                    model: req.body.model,
                    taskTypes: req.body.taskTypes,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
        })

        it('should not work with invalid model', async () => {
            const body = {
                code: '%%%%%%',
                brand: 'Apple',
                model: undefined,
                taskTypes: ['DELIVERY'],
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotTypeMap, 'toDTO').returns({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskTypes: req.body.taskTypes,
            })

            let repo = Container.get('RobotTypeRepo') as IRobotTypeRepo
            sandbox.stub(repo, 'exists').resolves(false)
            sandbox.stub(repo, 'save').resolves({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskType: req.body.taskTypes,
            } as RobotType)

            const service = Container.get('RobotTypeService') as IRobotTypeService
            const serviceSpy = sinon.spy(service, 'createRobotType')

            const ctrl = new RobotTypeController(service)
            await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.body.code,
                    brand: req.body.brand,
                    model: req.body.model,
                    taskTypes: req.body.taskTypes,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
        })

        it('should not work with invalid taskTypes', async () => {
            const body = {
                code: '%%%%%%',
                brand: 'Apple',
                model: 'X11',
                taskTypes: undefined,
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotTypeMap, 'toDTO').returns({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskTypes: req.body.taskTypes,
            })

            let repo = Container.get('RobotTypeRepo') as IRobotTypeRepo
            sandbox.stub(repo, 'exists').resolves(false)
            sandbox.stub(repo, 'save').resolves({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskType: req.body.taskTypes,
            } as RobotType)

            const service = Container.get('RobotTypeService') as IRobotTypeService
            const serviceSpy = sinon.spy(service, 'createRobotType')

            const ctrl = new RobotTypeController(service)
            await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.body.code,
                    brand: req.body.brand,
                    model: req.body.model,
                    taskTypes: req.body.taskTypes,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
        })

        it('should not work if already exists', async () => {
            const body = {
                code: '%%%%%%',
                brand: 'Apple',
                model: 'X11',
                taskTypes: ['DELIVERY'],
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotTypeMap, 'toDTO').returns({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskTypes: req.body.taskTypes,
            })

            let repo = Container.get('RobotTypeRepo') as IRobotTypeRepo
            sandbox.stub(repo, 'exists').resolves(true)
            sandbox.stub(repo, 'save').resolves({
                code: req.body.code,
                brand: req.body.brand,
                model: req.body.model,
                taskType: req.body.taskTypes,
            } as RobotType)

            const service = Container.get('RobotTypeService') as IRobotTypeService
            const serviceSpy = sinon.spy(service, 'createRobotType')

            const ctrl = new RobotTypeController(service)
            await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.body.code,
                    brand: req.body.brand,
                    model: req.body.model,
                    taskTypes: req.body.taskTypes,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(422))
        })
    })
})
