import 'reflect-metadata'

import * as sinon from 'sinon'
import { Response, Request, NextFunction } from 'express'
import { Container } from 'typedi';

import RobotType from '../src/domain/robotType/robotType'
import RobotTypeController from '../src/controllers/robotTypeController'
import IRobotTypeService from '../src/services/IServices/IRobotTypeService'
import { RobotTypeMap } from '../src/mappers/RobotTypeMap';
import { TaskType } from '../src/domain/robotType/taskType';

describe('robotType controller', function () {
    const sandbox = sinon.createSandbox()

    beforeEach(function () {
        // Container.reset()
        // let robotTypeSchema = require("../src/persistence/schemas/robotTypeSchema").default;
        // Container.set("robotType", robotTypeSchema);
    
        // let robotTypeRepoClass = require("../src/repos/robotTypeRepo").default;
        // let robotTypeRepo = Container.get(robotTypeRepoClass);
        // Container.set("RobotTypeRepo", robotTypeRepo);    
        
        // let robotTypeServiceClass = require("../src/services/robotTypeService").default
        // let robotTypeServiceInstance = Container.get(robotTypeServiceClass)
        // Container.set("RobotTypeService", robotTypeServiceInstance)
    })

    afterEach(function () {
        sandbox.restore()
    })

    describe('createRobotType()', function () {
        // TODO: finish testing
        // it('should fail with invalid parameters unit test using robotTypeService stub', async function () {
            
        //     // Arrange
        //     let body = { 
        //         "code": "Rob01",
        //         "brand": "brand",
        //         "model": "Model123",
        //         "taskType": ["DELIVERY"]
        //      }

        //     let req: Partial<Request> = {}
        //     req.body = body
        //     let res: Partial<Response> = {
        //         status: sandbox.spy()
        //     }
            
        //     let next: Partial<NextFunction> = () => { }
            
        //     let repo = Container.get("RobotTypeRepo") as IRobotTypeRepo

        //     sandbox.stub(repo, "save").resolves({
        //         code: req.body.code,
        //         brand: req.body.brand,
        //         model: req.body.model,
        //         taskType: req.body.taskTypes} as unknown as RobotType)

        //     sandbox.stub(RobotTypeMap, 'toDTO').returns({
        //         code: req.body.code,
        //         brand: req.body.nickname,
        //         model: req.body.typeCode,
        //         taskTypes: req.body.serialNumber,
        //     })

        //     sandbox.stub(TaskType, 'toType').returns(TaskType.DELIVERY)

        //     const robotTypeRepo = Container.get("RobotTypeRepo") as IRobotTypeRepo
        //     sandbox.stub(robotTypeRepo, "exists").resolves(false)

        //     let robotTypeServiceInstance = Container.get("RobotTypeService") as IRobotTypeService
        //     const serviceSpy = sandbox.spy(robotTypeServiceInstance, "createRobotType")

        //     const ctrl = new RobotTypeController(robotTypeServiceInstance as IRobotTypeService)

        //     // Act
        //     await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next)
        //     sandbox.assert.calledOnce(serviceSpy)

        //     sandbox.assert.calledWith(
        //         serviceSpy,
        //         sandbox.match({
        //             brand: req.body.nickname,
        //             code: req.body.code,
        //             model: req.body.typeCode,
        //             taskTypes: req.body.serialNumber,
        //         })
        //     )

        //     sandbox.assert.calledOnce(res.status as sinon.SinonSpy)
        //     sandbox.assert.calledWith(
        //         res.status as sinon.SinonSpy,
        //         sandbox.match(201)
        //     )
        // })
    })
})


