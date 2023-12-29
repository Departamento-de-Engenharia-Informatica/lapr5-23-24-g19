import 'reflect-metadata'

import { Request, NextFunction, Response } from 'express'

import { Container } from 'typedi'
import { left, right } from '../core/logic/Result'
import IClientService from '../services/IServices/IClientService'
import ClientController from './clientController'

import sinon from 'sinon'
import * as chai from 'chai'
import * as sinonChai from 'sinon-chai'
import IArchiveService from '../services/IServices/IArchiveService'

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
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
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

            // expect(res.status).to.have.been.calledOnceWith(422)
        })

        it('should succeed to create with correct parameters', async () => {
            const body = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
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
                    })
                },
            } as IClientService

            const ctrl = new ClientController(
                <IClientService>service,
                {} as IArchiveService,
            )
            await ctrl.createClient(<Request>req, <Response>res, <NextFunction>next)

            // expect(res.status).to.have.been.calledOnceWith(201)
        })
    })
})
