import 'reflect-metadata'

import { expect } from 'chai'
import { createSandbox } from 'sinon'
import Container from 'typedi'
import Building from '../src/domain/building/building'
import { IBuildingDTO } from '../src/dto/IBuildingDTO'
import { IBuildingEditDTO } from '../src/dto/IBuildingEditDTO'
import { BuildingMap } from '../src/mappers/BuildingMap'

import BuildingService from '../src/services/buildingService'
import IBuildingRepo from '../src/services/IRepos/IBuildingRepo'
import IFloorRepo from '../src/services/IRepos/IFloorRepo'
import { ErrorCode, ErrorResult } from '../src/services/IServices/IBuildingService'

describe('Building Service', () => {
    const sinon = createSandbox()

    beforeEach(() => {
        Container.reset()

        let schema = require('../src/persistence/schemas/buildingSchema').default
        Container.set('buildingSchema', schema)

        let repoC = require('../src/repos/mongo/buildingRepo').default
        let repoInst = Container.get(repoC)
        Container.set('BuildingRepo', repoInst)
    })

    afterEach(() => sinon.restore())

    describe('createBuilding()', () => {
        it('should fail if the code is not valid', async () => {
            const dto: IBuildingDTO = {
                code: '#BuildingA',
                maxFloorDimensions: { length: 20, width: 30 },
            }

            const svc = new BuildingService(
                {} as IBuildingRepo, // should not be needed here
                {} as IFloorRepo, // not needed here
            )

            const result = await svc.createBuilding(dto)
            expect(result.isLeft()).to.be.true

            const err = result.value as ErrorResult
            expect(err.errorCode).to.equal(ErrorCode.BusinessRuleViolation)
        })

        it('should succeed if the params are valid', async () => {
            const dto: IBuildingDTO = {
                code: 'A',
                maxFloorDimensions: { length: 20, width: 30 },
            }

            const repo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(repo, 'findByCode').resolves(null as unknown as Building)
            sinon.stub(repo, 'save').resolves({} as Building)

            sinon.stub(BuildingMap, 'toDTO').returns({
                code: 'A',
                maxFloorDimensions: { length: 20, width: 30 },
            })

            const svc = new BuildingService(
                repo, // should not be needed here
                {} as IFloorRepo, // not needed here
            )

            const result = await svc.createBuilding(dto)
            expect(result.isRight()).to.be.true

            const res = result.value as IBuildingDTO
            expect(res).to.deep.equal(dto)
        })

        it('should fail if the building already exists', async () => {
            const dto: IBuildingDTO = {
                code: 'A',
                maxFloorDimensions: { length: 20, width: 30 },
            }

            const repo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(repo, 'findByCode').resolves({} as Building)

            const svc = new BuildingService(
                repo, // should not be needed here
                {} as IFloorRepo, // not needed here
            )

            const result = await svc.createBuilding(dto)
            expect(result.isLeft()).to.be.true

            const err = result.value as ErrorResult
            expect(err.errorCode).to.equal(ErrorCode.AlreadyExists)
        })
    })

    describe('getBuilding()', () => {
        it('should fail if building not found', async () => {
            const dto = 'A'

            const repo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(repo, 'findByCode').resolves(null as unknown as Building)

            const svc = new BuildingService(
                repo, // should not be needed here
                {} as IFloorRepo, // not needed here
            )

            const result = await svc.getBuilding(dto)
            expect(result.isLeft()).to.be.true

            const err = result.value as ErrorResult
            expect(err.errorCode).to.equal(ErrorCode.NotFound)
        })

        it('should succeed if building found', async () => {
            const dto = 'A'

            const repo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(repo, 'findByCode').resolves({} as Building)
            sinon.stub(BuildingMap, 'toDTO').returns({
                code: 'A',
                name: 'Building A',
                description: 'IT',
                maxFloorDimensions: { length: 150, width: 40 },
            })

            const svc = new BuildingService(
                repo, // should not be needed here
                {} as IFloorRepo, // not needed here
            )

            const result = await svc.getBuilding(dto)
            expect(result.isRight()).to.be.true

            const res = result.value as IBuildingDTO
            expect(res.code).to.equal(dto)
            expect(res).to.deep.equal({
                code: 'A',
                name: 'Building A',
                description: 'IT',
                maxFloorDimensions: { length: 150, width: 40 },
            })
        })
    })

    // describe('editBuilding()', () => {
    //     it('should fail if the code is not valid', async () => {
    //         const dto: IBuildingDTO = {
    //             code: '#BuildingA',
    //             maxFloorDimensions: { length: 20, width: 30 }
    //         }

    //         const svc = new BuildingService(
    //             {} as IBuildingRepo, // should not be needed here
    //             {} as IFloorRepo // not needed here
    //         )

    //         const result = await svc.createBuilding(dto)
    //         expect(result.isLeft()).to.be.true

    //         const err = result.value as ErrorResult
    //         expect(err.errorCode).to.equal(ErrorCode.BusinessRuleViolation)
    //     })

    //     it('should succeed if the params are valid', async () => {
    //         const dto: IBuildingEditDTO = {
    //             code: 'A',
    //             description: 'Hello world'
    //         }

    //         const repo = Container.get('BuildingRepo') as IBuildingRepo
    //         sinon.stub(repo, 'findByCode').resolves({
    //             code: 'A',
    //             description: 'Building A',
    //             maxFloorDimensions: { length: 20, width: 20 }
    //         } as unknown as Building)
    //         sinon.stub(repo, 'save').rejects()

    //         sinon.stub(BuildingMap, 'toDTO').returns({
    //             code: 'A',
    //             maxFloorDimensions: { length: 20, width: 30 }
    //         })

    //         const svc = new BuildingService(
    //             repo, // should not be needed here
    //             {} as IFloorRepo // not needed here
    //         )

    //         const result = await svc.editBuilding(dto)
    //         expect(result.isRight()).to.be.true

    //         const res = result.value as IBuildingDTO
    //         expect(res).to.deep.equal(dto)
    //     })

    //     it('should fail if the building already exists', async () => {
    //         const dto: IBuildingDTO = {
    //             code: 'A',
    //             maxFloorDimensions: { length: 20, width: 30 }
    //         }

    //         const repo = Container.get('BuildingRepo') as IBuildingRepo
    //         sinon.stub(repo, 'findByCode').resolves({} as Building)

    //         const svc = new BuildingService(
    //             repo, // should not be needed here
    //             {} as IFloorRepo // not needed here
    //         )

    //         const result = await svc.createBuilding(dto)
    //         expect(result.isLeft()).to.be.true

    //         const err = result.value as ErrorResult
    //         expect(err.errorCode).to.equal(ErrorCode.AlreadyExists)
    //     })
    // })
})
