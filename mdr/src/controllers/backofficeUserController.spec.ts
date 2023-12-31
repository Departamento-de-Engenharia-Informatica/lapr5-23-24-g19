import 'reflect-metadata'

import { Request, NextFunction, Response } from 'express'

import { Container } from 'typedi'
import { left, right } from '../core/logic/Result'

import sinon from 'sinon'
import * as chai from 'chai'
import * as sinonChai from 'sinon-chai'
import IBackofficeUserService from '../services/IServices/IBackofficeUserService'
import BackofficeUserController from './backofficeUserController'

chai.use(sinonChai.default)

const { expect } = chai

describe('BackofficeUser controller Unit', () => {
    const sandbox = sinon.createSandbox()

    beforeEach(function () {
        Container.reset()
    })

    afterEach(function () {
        sandbox.restore()
    })

    describe('createBackofficeUser()', () => {
        it('should fail to create with bad parameters', async () => {
            const body = {
                email: 'mzc@isep.ipp.pt',
                role: 'Fleet Manager',
                name: 'Maria',
                phoneNumber: '9165874',
                password: 'Password1$',
            }

            const req: Partial<Request> = {}
            req.body = body as unknown as NodeJS.ReadableStream

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }
            const next: Partial<NextFunction> = () => {}

            const service: Partial<IBackofficeUserService> = {
                async createBackofficeUser(_dto) {
                    return left({
                        errorCode: 1,
                        message: 'bad',
                    })
                },
            } as IBackofficeUserService

            const ctrl = new BackofficeUserController(<IBackofficeUserService>service)
            await ctrl.createBackofficeUser(
                <Request>req,
                <Response>res,
                <NextFunction>next,
            )

            expect(res.status).to.have.been.calledOnceWith(422)
        })

        it('should succeed to create with correct parameters', async () => {
            const body = {
                email: 'mzc@isep.ipp.pt',
                role: 'Fleet Manager',
                name: 'Maria',
                phoneNumber: '912119482',
                password: 'Password1$',
            }

            const req: Partial<Request> = {}

            req.body = body as unknown as NodeJS.ReadableStream

            const res: Partial<Response> = {
                status: sandbox.stub().returnsThis(),
                send: sandbox.stub(),
            }
            const next: Partial<NextFunction> = () => {}

            const service: Partial<IBackofficeUserService> = {
                async createBackofficeUser(_dto) {
                    return right({
                        email: 'mzc@isep.ipp.pt',
                        role: 'Fleet Manager',
                        name: 'Maria',
                        phoneNumber: '912119482',
                    })
                },
            } as IBackofficeUserService

            const ctrl = new BackofficeUserController(<IBackofficeUserService>service)
            await ctrl.createBackofficeUser(
                <Request>req,
                <Response>res,
                <NextFunction>next,
            )

            expect(res.status).to.have.been.calledOnceWith(201)
        })
    })
})
