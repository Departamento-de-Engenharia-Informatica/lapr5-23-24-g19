import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import IRobotRepo from '../src/repos/mongo/robotRepo'
import Robot from '../src/domain/robot/Robot';
import IRobotTypeRepo from '../src/services/IRepos/IRobotTypeRepo';
import RobotType from '../src/domain/robotType/robotType';
import IRobotService from '../src/services/IServices/IRobotService'
import RobotController from '../src/controllers/robotController'
import { RobotMap } from '../src/mappers/RobotMap'

describe('Robot controller:', () => {
	const sandbox = sinon.createSandbox();

	beforeEach(function() {
		Container.reset();

        let robotTypeSchema = require("../src/persistence/schemas/robotTypeSchema").default;
        Container.set("robotType", robotTypeSchema);

        let robotTypeRepoClass = require("../src/repos/robotTypeRepo").default;
        let robotTypeRepo = Container.get(robotTypeRepoClass);
        Container.set("RobotTypeRepo", robotTypeRepo);

		let robotSchema = require("../src/persistence/schemas/robotSchema").default;
		Container.set("robotSchema", robotSchema);

		let robotRepoClass = require("../src/repos/mongo/robotRepo").default;
		let robotRepo = Container.get(robotRepoClass);
		Container.set("RobotRepo", robotRepo);

		let serviceClass = require("../src/services/robotService").default;
		let service = Container.get(serviceClass);
		Container.set("RobotService", service);
    });

	afterEach(function() {
		sandbox.restore();
	});

    describe('createRobot(): robotController + robotService integration test using spy on roleService', () => {
        it('should work with correct values', async () => {
            const body = {
                "code": 'Robot12',
                "nickname": "Marco",
                "typeCode": "Optimus",
                "serialNumber": "SERIAL12",
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy()
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotMap, 'toDTO').returns({
                    "code": req.body.code,
                    "nickname": req.body.nickname,
                    "typeCode": req.body.typeCode,
                    "serialNumber": req.body.serialNumber,
                    "state": "Enabled"
            })

            let robotRepo = Container.get("RobotRepo") as IRobotRepo
            sandbox.stub(robotRepo, "save").resolves({
                "code": req.body.code,
                "nickname": req.body.nickname,
                "typeCode": req.body.typeCode,
                "serialNumber": req.body.serialNumber,
                "state": 0
            } as unknown as Robot)

            let robotTypeRepo = Container.get("RobotTypeRepo") as IRobotTypeRepo
            sandbox.stub(robotTypeRepo, "find").resolves({
                "code": 'Robot12',
            } as unknown as RobotType)

            const service = Container.get("RobotService") as IRobotService
            const serviceSpy = sinon.spy(service, "createRobot")

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.createRobot(<Request> req, <Response> res, <NextFunction> next)

            sandbox.assert.calledOnce(serviceSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    "code": req.body.code,
                    "nickname": req.body.nickname,
                    "typeCode": req.body.typeCode,
                    "serialNumber": req.body.serialNumber,
                })
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                res.status as sinon.SinonSpy,
                sandbox.match(201)
            )
        })

        it('should not work with invalid typeCode', async () => {
            const body = {
                "code": 'Robot12',
                "nickname": "Marco",
                "typeCode": "Invalid",
                "serialNumber": "SERIAL12",
            }

            const req: Partial<Request> = {}
            req.body = body

            const res: Partial<Response> = {
                status: sandbox.spy()
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotMap, 'toDTO').returns({
                    "code": req.body.code,
                    "nickname": req.body.nickname,
                    "typeCode": req.body.typeCode,
                    "serialNumber": req.body.serialNumber,
                    "state": "Enabled"
            })

            let robotRepo = Container.get("RobotRepo") as IRobotRepo
            sandbox.stub(robotRepo, "save").resolves({
                "code": req.body.code,
                "nickname": req.body.nickname,
                "typeCode": req.body.typeCode,
                "serialNumber": req.body.serialNumber,
                "state": 0
            } as unknown as Robot)

            let robotTypeRepo = Container.get("RobotTypeRepo") as IRobotTypeRepo
            sandbox.stub(robotTypeRepo, "find").resolves(undefined)

            const service = Container.get("RobotService") as IRobotService
            const serviceSpy = sinon.spy(service, "createRobot")

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.createRobot(<Request> req, <Response> res, <NextFunction> next)

            sandbox.assert.calledOnce(serviceSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    "code": req.body.code,
                    "nickname": req.body.nickname,
                    "typeCode": req.body.typeCode,
                    "serialNumber": req.body.serialNumber,
                })
            )

            sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
            sandbox.assert.calledWith(
                res.status as sinon.SinonSpy,
                sandbox.match(404)
            )
        })
    })

    describe('createRobot(): robotController + robotService integration test using spy on roleService', () => {
        it('should work with correct values', async () => {
            const body = {
                "id": 'Robot12',
            }

            const req: Partial<Request> = {}
            req.params = body

            const res: Partial<Response> = {
                status: sandbox.spy(),
                json: sandbox.spy()
            }

            const next: Partial<NextFunction> = () => {}

            sandbox.stub(RobotMap, 'toDTO').returns({
                    "code": req.params.id,
                    "nickname": 'test',
                    "typeCode": '123h',
                    "serialNumber": 'SERIAL123',
                    "state": "Disabled"
            })

            let robotRepo = Container.get("RobotRepo") as IRobotRepo
            sandbox.stub(robotRepo, "find").resolves({
                "code": req.params.id,
                "state": 0,
                "inhibit": function() {
                    this.state = 1
                }
            } as unknown as Robot)

            sandbox.stub(robotRepo, "save").resolves({
                "code": req.params.id,
                "state": 1
            } as unknown as Robot)

            const service = Container.get("RobotService") as IRobotService
            const serviceSpy = sandbox.spy(service, "inhibitRobot")

            const ctrl = new RobotController(service as IRobotService)

            await ctrl.inhibitRobot(<Request> req, <Response> res, <NextFunction> next)

            sandbox.assert.calledOnce(serviceSpy)
            sandbox.assert.calledWith(
                serviceSpy,
                sandbox.match({
                    "code": req.params.id,
                })
            )

            sandbox.assert.calledOnce(res.json as sinon.SinonSpy)
            sandbox.assert.calledWith(
                res.json as sinon.SinonSpy,
                sandbox.match({
                    "code": req.params.id,
                    "nickname": 'test',
                    "typeCode": '123h',
                    "serialNumber": 'SERIAL123',
                    "state": "Disabled"
                })
            )
        })
    })
})
