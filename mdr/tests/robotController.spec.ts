import 'reflect-metadata'

import { NextFunction, Request, Response } from 'express'
import * as sinon from 'sinon'
import { Container } from 'typedi'
import RobotController from '../src/controllers/robotController'
import Robot from '../src/domain/robot/Robot'
import RobotType from '../src/domain/robotType/robotType'
import { RobotMap } from '../src/mappers/RobotMap'
import IRobotRepo from '../src/repos/mongo/robotRepo'
import IRobotTypeRepo from '../src/services/IRepos/IRobotTypeRepo'
import IRobotService from '../src/services/IServices/IRobotService'

describe('Robot controller:', () => {
    const sandbox = sinon.createSandbox()

    beforeEach(function () {
        Container.reset()

        let robotTypeSchema =
            require('../src/persistence/schemas/robotTypeSchema').default
        Container.set('robotType', robotTypeSchema)

        let robotTypeRepoClass = require('../src/repos/mongo/robotTypeRepo').default
        let robotTypeRepo = Container.get(robotTypeRepoClass)
        Container.set('RobotTypeRepo', robotTypeRepo)

        let robotSchema = require('../src/persistence/schemas/robotSchema').default
        Container.set('robotSchema', robotSchema)

        let robotRepoClass = require('../src/repos/mongo/robotRepo').default
        let robotRepo = Container.get(robotRepoClass)
        Container.set('RobotRepo', robotRepo)

        let serviceClass = require('../src/services/robotService').default
        let service = Container.get(serviceClass)
        Container.set('RobotService', service)
    })

    afterEach(function () {
        sandbox.restore()
    })

    describe('createRobot(): robotController + robotService integration test using spy on robotService', () => {
        it('should work with correct values', async () => {
            const body = {
                code: 'Robot12',
                nickname: 'Marco',
                typeCode: 'Optimus',
                serialNumber: 'SERIAL12',
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotMap, 'toDTO').returns({
                code: req.body.code,
                nickname: req.body.nickname,
                typeCode: req.body.typeCode,
                serialNumber: req.body.serialNumber,
                state: 0,
            })

            let robotRepo = Container.get('RobotRepo') as IRobotRepo
            sandbox.stub(robotRepo, 'find').resolves(null as unknown as Robot)
            sandbox.stub(robotRepo, 'save').resolves({
                code: req.body.code,
                nickname: req.body.nickname,
                typeCode: req.body.typeCode,
                serialNumber: req.body.serialNumber,
                state: 0,
            } as unknown as Robot)

            let robotTypeRepo = Container.get('RobotTypeRepo') as IRobotTypeRepo
            sandbox.stub(robotTypeRepo, 'find').resolves({
                code: 'Robot12',
            } as unknown as RobotType)

            const service = Container.get('RobotService') as IRobotService
            const serviceSpy = sinon.spy(service, 'createRobot')

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.createRobot(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(serviceSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.body.code,
                    nickname: req.body.nickname,
                    typeCode: req.body.typeCode,
                    serialNumber: req.body.serialNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(201))
        })

        it('should not work with invalid typeCode', async () => {
            const body = {
                code: 'Robot12',
                nickname: 'Marco',
                typeCode: 'Invalid',
                serialNumber: 'SERIAL12',
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotMap, 'toDTO').returns({
                code: req.body.code,
                nickname: req.body.nickname,
                typeCode: req.body.typeCode,
                serialNumber: req.body.serialNumber,
                state: 0,
            })

            let robotRepo = Container.get('RobotRepo') as IRobotRepo
            sandbox.stub(robotRepo, 'find').resolves(null as unknown as Robot)
            sandbox.stub(robotRepo, 'save').resolves({
                code: req.body.code,
                nickname: req.body.nickname,
                typeCode: req.body.typeCode,
                serialNumber: req.body.serialNumber,
                state: 0,
            } as unknown as Robot)

            let robotTypeRepo = Container.get('RobotTypeRepo') as IRobotTypeRepo
            sandbox.stub(robotTypeRepo, 'find').resolves(undefined)

            const service = Container.get('RobotService') as IRobotService
            const serviceSpy = sinon.spy(service, 'createRobot')

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.createRobot(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(serviceSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.body.code,
                    nickname: req.body.nickname,
                    typeCode: req.body.typeCode,
                    serialNumber: req.body.serialNumber,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(404))
        })
    })

    describe('inhibitRobot(): robotController + robotService integration test using spy on robotService', () => {
        it('should work with correct values', async () => {
            const req: Partial<Request> = {
                body: { state: 0 },
            }
            req.params = { id: 'Robot12' }

            const res: Partial<Response> = {
                status: sandbox.spy(),
                json: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotMap, 'toDTO').returns({
                code: req.params.id,
                nickname: 'test',
                typeCode: '123h',
                serialNumber: 'SERIAL123',
                state: 1,
            })

            let robotRepo = Container.get('RobotRepo') as IRobotRepo
            sandbox.stub(robotRepo, 'find').resolves({
                code: req.params.id,
                state: 0,
                inhibit: function () {
                    this.state = 1
                },
            } as unknown as Robot)

            sandbox.stub(robotRepo, 'save').resolves({
                code: req.params.id,
                state: 1,
            } as unknown as Robot)

            const service = Container.get('RobotService') as IRobotService
            const serviceSpy = sandbox.spy(service, 'inhibitRobot')

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.inhibitRobot(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(serviceSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.params.id,
                    state: req.body!.state,
                }),
            )

            // sandbox.assert.calledOnce(res.json as sinon.SinonSpy)
            // sandbox.assert.calledWith(
            //     res.json as sinon.SinonSpy,
            //     sandbox.match({
            //         code: req.params.id,
            //         nickname: 'test',
            //         typeCode: '123h',
            //         serialNumber: 'SERIAL123',
            //         state: 1,
            //     }),
            // )
        })

        it('should not work when robot not found', async () => {
            const req: Partial<Request> = {
                body: { state: 0 },
            }
            req.params = { id: 'Robot12' }

            const res: Partial<Response> = {
                status: sandbox.spy(),
                json: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotMap, 'toDTO').returns({
                code: req.params.id,
                nickname: 'test',
                typeCode: '123h',
                serialNumber: 'SERIAL123',
                state: 1,
            })

            let robotRepo = Container.get('RobotRepo') as IRobotRepo
            sandbox.stub(robotRepo, 'find').resolves(undefined)

            const service = Container.get('RobotService') as IRobotService
            const serviceSpy = sandbox.spy(service, 'inhibitRobot')

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.inhibitRobot(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(serviceSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    code: req.params.id,
                    state: req.body!.state,
                }),
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(404))
        })
    })
    describe('getRobots(): robotController + robotService integration test using spy on robotService', () => {
        it('should work if there are robot persisted', async () => {
            const req: Partial<Request> = {}
            const res: Partial<Response> = {
                status: sandbox.spy(),
                json: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            let robotRepo = Container.get('RobotRepo') as IRobotRepo
            sandbox.stub(robotRepo, 'findAll').resolves([
                {
                    code: 'ROBOT123',
                    nickname: 'test',
                    type: '123h',
                    serialNumber: 'SERIAL123',
                    state: 1,
                },
            ] as unknown as Robot[])

            sandbox.stub(RobotMap, 'toDTO').returns({
                code: 'ROBOT123',
                nickname: 'test',
                typeCode: '123h',
                serialNumber: 'SERIAL123',
                state: 1,
            })

            const service = Container.get('RobotService') as IRobotService
            const serviceSpy = sandbox.spy(service, 'getRobots')

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.getRobots(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(serviceSpy)

            sandbox.assert.calledOnce(res.json as sinon.SinonSpy)
            sandbox.assert.calledWith(
                res.json as sinon.SinonSpy,
                sandbox.match([
                    {
                        code: 'ROBOT123',
                        nickname: 'test',
                        serialNumber: 'SERIAL123',
                        state: 1,
                        typeCode: '123h',
                    },
                ]),
            )
        })

        it('should not work if robots are not found', async () => {
            const req: Partial<Request> = {}
            const res: Partial<Response> = {
                status: sandbox.spy(),
                json: sandbox.spy(),
            }

            const next: Partial<NextFunction> = () => {}

            let robotRepo = Container.get('RobotRepo') as IRobotRepo
            sandbox.stub(robotRepo, 'findAll').resolves([])

            const service = Container.get('RobotService') as IRobotService
            const serviceSpy = sandbox.spy(service, 'getRobots')

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.getRobots(<Request>req, <Response>res, <NextFunction>next)

            sandbox.assert.calledOnce(serviceSpy)

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(res.status as sinon.SinonSpy, sandbox.match(404))
        })
    })
})
