import 'reflect-metadata'

import { Request, NextFunction, Response } from 'express'

import { Container } from 'typedi'
import { left, right } from '../core/logic/Result'
import IClientService from '../services/IServices/IClientService'
import IArchiveService from '../services/IServices/IArchiveService'
import ClientController from './clientController'

import sinon from 'sinon'
import * as chai from 'chai'
import * as sinonChai from 'sinon-chai'

chai.use(sinonChai.default)

const { expect } = chai

describe('Client controller Unit', () => {
    const sandbox = sinon.createSandbox()

    beforeEach(function () {
        Container.reset()
    })

    afterEach(function () {
        sandbox.restore()
    })

    describe('createClient()', () => {
        it('should fail to create with bad parameters', async () => {
            const body = {
                name: 'Maria',
                email: 'mzc@isep.ipp.pt',
                phoneNumber: '91229',
                vatNumber: 1102,
                password: 'Password1$',
            }

            const req: Partial<Request> = {}
            req.body = body as unknown as NodeJS.ReadableStream

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }
            const next: Partial<NextFunction> = () => {}

            const service: Partial<IClientService> = {
                async createClient(_dto) {
                    return left({
                        errorCode: 1,
                        message: 'bad',
                    })
                },
            } as IClientService

            const ctrl = new ClientController(
                <IClientService>service,
                {} as IArchiveService,
            )
            await ctrl.createClient(<Request>req, <Response>res, <NextFunction>next)

            expect(res.status).to.have.been.calledOnceWith(422)
        })

        it('should succeed to create with correct parameters', async () => {
            const body = {
                name: 'Maria',
                email: 'mzc@isep.ipp.pt',
                phoneNumber: '912119482',
                vatNumber: 110212558,
                password: 'Password1$',
            }

            const req: Partial<Request> = {}

            req.body = body as unknown as NodeJS.ReadableStream

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }
            const next: Partial<NextFunction> = () => {}

            const service: Partial<IClientService> = {
                async createClient(_dto) {
                    return right({
                        email: 'mzc@isep.ipp.pt',
                        name: 'Maria',
                        phoneNumber: '912119482',
                        vatNumber: 110212558,
                        status: 'Pending',
                    })
                },
            } as IClientService

            const ctrl = new ClientController(
                <IClientService>service,
                {} as IArchiveService,
            )
            await ctrl.createClient(<Request>req, <Response>res, <NextFunction>next)

            expect(res.status).to.have.been.calledOnceWith(201)
        })
    })

    describe('updateClientState()', () => {
        it('should fail to update with bad parameters', async () => {
            const body = {
                email: '1211155@isep.ipp',
                state: 'invalid',
            }

            const req: Partial<Request> = {}
            req.body = body as unknown as NodeJS.ReadableStream

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }

            const next: Partial<NextFunction> = () => {}

            const ctrl = new ClientController({} as IClientService, {} as IArchiveService)
            await ctrl.updateClientState(<Request>req, <Response>res, <NextFunction>next)

            expect(res.status).to.have.been.calledOnceWith(400)
        })

        it('should succeed to update with correct parameters', async () => {
            const body = {
                email: '1211155@isep.ipp.pt',
                state: 'approved',
            }

            const req: Partial<Request> = {}
            req.body = body as unknown as NodeJS.ReadableStream

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }

            const next: Partial<NextFunction> = () => {}

            const service: Partial<IClientService> = {
                async updateClientState(_dto) {
                    return right({
                        email: '1211155@isep.ipp.pt',
                        name: 'Maria',
                        phoneNumber: '912119482',
                        vatNumber: 110212558,
                        status: 'Approved',
                    })
                },
            } as IClientService

            const ctrl = new ClientController(
                <IClientService>service,
                {} as IArchiveService,
            )
            await ctrl.updateClientState(<Request>req, <Response>res, <NextFunction>next)

            expect(res.status).to.have.been.calledOnceWith(200)
        })
    })

    describe('getClient()', () => {
        it('should be able to get client', async () => {
            const req: Partial<Request> = {}
            req.params = {
                email: 'nvm@isep.ipp.pt',
            }

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }

            const next: Partial<NextFunction> = () => {}

            const service: Partial<IClientService> = {
                async getClient(_email) {
                    return right({
                        email: 'nvm@isep.ipp.pt',
                        name: 'Maria',
                        phoneNumber: '912119482',
                        vatNumber: 110212558,
                        status: 'Approved',
                    })
                },
            } as IClientService

            const ctrl = new ClientController(
                <IClientService>service,
                {} as IArchiveService,
            )
            await ctrl.getClient(<Request>req, <Response>res, <NextFunction>next)

            expect(res.status).to.have.been.calledOnceWith(200)
        })
    })

    describe('patchClient()', () => {
        it('should be able to patch client', async () => {
            const req: Partial<Request> = {}
            req.params = {
                email: '1211155@isep.ipp.pt',
            }

            req.body = {
                email: '1211155@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912119482',
                vatNumber: '110212558',
            }

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }

            const next: Partial<NextFunction> = () => {}

            const service: Partial<IClientService> = {
                async patchClient(_dto) {
                    return right({
                        email: '1211155@isep.ipp.pt',
                        name: 'Maria',
                        phoneNumber: '912119482',
                        vatNumber: 110212558,
                    })
                },
            } as IClientService

            const ctrl = new ClientController(
                <IClientService>service,
                {} as IArchiveService,
            )
            await ctrl.patchClient(<Request>req, <Response>res, <NextFunction>next)

            expect(res.status).to.have.been.calledOnceWith(200)
        })

        it('should fail to patch client with bad parameters', async () => {
            const req: Partial<Request> = {}
            req.params = {
                email: 'fdsafsda',
            }
            req.body = {
                email: 'fdsafsda',
                name: 'Maria',
                phoneNumber: '912119482433452',
            }

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }

            const next: Partial<NextFunction> = () => {}

            const service: Partial<IClientService> = {
                async patchClient(_dto) {
                    return left({
                        errorCode: 1,
                        message: 'bad',
                    })
                },
            } as IClientService

            const ctrl = new ClientController(
                <IClientService>service,
                {} as IArchiveService,
            )
            await ctrl.patchClient(<Request>req, <Response>res, <NextFunction>next)

            expect(res.status).to.have.been.calledOnceWith(422)
        })
    })
})
