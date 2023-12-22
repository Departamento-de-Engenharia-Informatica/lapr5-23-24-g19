import 'reflect-metadata'

import { expect } from 'chai'
import { describe, it } from 'mocha'
import { createSandbox } from 'sinon'
import Container from 'typedi'
import IBackofficeUserRepo from '../src/services/IRepos/IBackofficeUserRepo'
import BackofficeUserService from '../src/services/backofficeUserService'
import { ICreatedBackofficeUserDTO } from '../src/dto/ICreatedBackofficeUserDTO'
import { BackofficeUserMap } from '../src/mappers/BackofficeUserMap'
import { IBackofficeUserDTO } from '../src/dto/IBackofficeUserDTO'
import BackofficeUser from '../src/domain/user/backofficeUser/backofficeUser'

describe('Backoffice user Service: Integration tests', () => {
    const sinon = createSandbox()

    // function stubCreate<K>(klass: K) {
    //     sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    // }

    beforeEach(() => {
        Container.reset()

        const backofficeUserSchema = require('../src/persistence/schemas/backofficeUserSchema')
            .default
        Container.set('backofficeUserSchema', backofficeUserSchema)

        const backofficeUserRepoClass = require('../src/repos/mongo/backofficeUserRepo')
            .default
        const backofficeUserRepo = Container.get(backofficeUserRepoClass)
        Container.set('backofficeUserRepo', backofficeUserRepo)
    })

    afterEach(() => sinon.restore())

    describe('createBackofficeUser(): service + domain tests', () => {
        it('should fail if backoffice user exists', async () => {
            const backofficeUserRepo = Container.get(
                'BackofficeUserRepo',
            ) as IBackofficeUserRepo
            sinon.stub(backofficeUserRepo, 'existsWithEmail').resolves(true)
            sinon.stub(backofficeUserRepo, 'save').rejects()

            const dto: IBackofficeUserDTO = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912201029',
                password: 'Password1$',
            }

            const service = new BackofficeUserService(backofficeUserRepo)
            const result = await service.createBackofficeUser(dto)

            expect(result.isLeft()).to.be.true
        })

        it('should succeed with right parameters', async () => {
            const backofficeUserRepo = Container.get(
                'BackofficeUserRepo',
            ) as IBackofficeUserRepo
            sinon.stub(backofficeUserRepo, 'existsWithEmail').resolves(false)
            sinon
                .stub(backofficeUserRepo, 'save')
                .resolves(({} as unknown) as BackofficeUser)

            sinon
                .stub(BackofficeUserMap, 'toDTO')
                .returns(({} as unknown) as ICreatedBackofficeUserDTO)

            const dto: IBackofficeUserDTO = {
                email: 'mzc@isep.ipp.pt',
                name: 'Maria',
                phoneNumber: '912201029',
                password: 'Password1$',
            }

            const service = new BackofficeUserService(backofficeUserRepo)
            const result = await service.createBackofficeUser(dto)

            expect(result.isRight()).to.be.true
        })
    })
})
