import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../src/core/logic/Result';
import IRoleService from "../src/services/IServices/IRoleService";
import RoleController from "../src/controllers/roleController";
import IRoleDTO from '../src/dto/IRoleDTO';
import { Role } from '../src/domain/role';

describe('role controller', function () {
	const sandbox = sinon.createSandbox();

	beforeEach(function() {
		Container.reset();
		let roleSchemaInstance = require("../src/persistence/schemas/roleSchema").default;
		Container.set("roleSchema", roleSchemaInstance);

		let roleRepoClass = require("../src/repos/roleRepo").default;
		let roleRepoInstance = Container.get(roleRepoClass);
		Container.set("RoleRepo", roleRepoInstance);

		let roleServiceClass = require("../src/services/roleService").default;
		let roleServiceInstance = Container.get(roleServiceClass);
		Container.set("RoleService", roleServiceInstance);
    });

	afterEach(function() {
		sandbox.restore();
	});

    it('roleController unit test using roleService stub', async function () {
		// Arrange
        let body = { "name":'role12' };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let roleServiceInstance = Container.get("RoleService");
		sinon.stub(roleServiceInstance, "createRole").returns( Result.ok<IRoleDTO>( {"id":"123", "name": req.body.name} ));

		const ctrl = new RoleController(roleServiceInstance as IRoleService);

		// Act
		await ctrl.createRole(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ "id": "123","name": req.body.name}));
	});


    it('roleController + roleService integration test using roleRepoistory and Role stubs', async function () {	
		// Arrange	
        let body = { "name":'role12' };
        let req: Partial<Request> = {};
		req.body = body;

        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		sinon.stub(Role, "create").returns(Result.ok({"id":"123", "name": req.body.name}));

		let roleRepoInstance = Container.get("RoleRepo");
		sinon.stub(roleRepoInstance, "save").returns(new Promise<Role>((resolve, reject) => {
			resolve(Role.create({"id":"123", "name": req.body.name}).getValue())
		}));

		let roleServiceInstance = Container.get("RoleService");

		const ctrl = new RoleController(roleServiceInstance as IRoleService);

		// Act
		await ctrl.createRole(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ "id": "123","name": req.body.name}));
	});


    it('roleController + roleService integration test using spy on roleService', async function () {		
		// Arrange
        let body = { "name":'role12' };
        let req: Partial<Request> = {};
		req.body = body;

        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let roleRepoInstance = Container.get("RoleRepo");
		sinon.stub(roleRepoInstance, "save").returns(new Promise<Role>((resolve, reject) => {
			resolve(Role.create({"id":"123", "name": req.body.name}).getValue())
		}));

		let roleServiceInstance = Container.get("RoleService");		
		const roleServiceSpy = sinon.spy(roleServiceInstance, "createRole");

		const ctrl = new RoleController(roleServiceInstance as IRoleService);

		// Act
		await ctrl.createRole(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ "id": "123","name": req.body.name}));
		sinon.assert.calledOnce(roleServiceSpy);
		//sinon.assert.calledTwice(roleServiceSpy);
		sinon.assert.calledWith(roleServiceSpy, sinon.match({name: req.body.name}));
	});


    it('roleController unit test using roleService mock', async function () {		
		// Arrange
        let body = { "name":'role12' };
        let req: Partial<Request> = {};
		req.body = body;

        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let roleServiceInstance = Container.get("RoleService");		
		const roleServiceMock = sinon.mock(roleServiceInstance, "createRole")
		roleServiceMock.expects("createRole")
			.once()
			.withArgs(sinon.match({name: req.body.name}))
			.returns(Result.ok<IRoleDTO>( {"id":"123", "name": req.body.name} ));

		const ctrl = new RoleController(roleServiceInstance as IRoleService);

		// Act
		await ctrl.createRole(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		roleServiceMock.verify();
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ "id": "123","name": req.body.name}));
	});
});


