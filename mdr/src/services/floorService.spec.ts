import { assert } from 'chai'
import { describe, it } from 'mocha'
import { createSandbox } from 'sinon'
import Container from 'typedi'
import { Result } from '../core/logic/Result'
import Building from '../domain/building/building'
import { BuildingCode } from '../domain/building/code'
import { Description } from '../domain/description'
import { Floor } from '../domain/floor/floor'
import { FloorNumber } from '../domain/floor/floorNumber'
import { IFloorDTO } from '../dto/IFloorDTO'
import { IUpdateFloorDTO } from '../dto/IUpdateFloorDTO'
import { FloorMap } from '../mappers/FloorMap'
import FloorService from './floorService'
import IBuildingRepo from './IRepos/IBuildingRepo'
import IFloorRepo from './IRepos/IFloorRepo'
import IPassageRepo from './IRepos/IPassageRepo'

describe('Floor Service: Unit Tests', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    beforeEach(function () {
        Container.reset()

        let floorSchema = require('../persistence/schemas/floorSchema').default
        Container.set('floorSchema', floorSchema)

        let buildingSchema = require('../persistence/schemas/buildingSchema').default
        Container.set('buildingSchema', buildingSchema)

        let passageSchema = require('../persistence/schemas/passageSchema').default
        Container.set('passageSchema', passageSchema)

        let buildingRepoClass = require('../repos/mongo/buildingRepo').default
        let buildingRepo = Container.get(buildingRepoClass)
        Container.set('BuildingRepo', buildingRepo)

        let floorRepoClass = require('../repos/mongo/floorRepo').default
        let floorRepo = Container.get(floorRepoClass)
        Container.set('FloorRepo', floorRepo)

        let passageRepoClass = require('../repos/mongo/passageRepo').default
        let passageRepo = Container.get(passageRepoClass)
        Container.set('passageRepo', passageRepo)
    })

    afterEach(sinon.restore)

    describe('createFloor()', () => {
        it('should work with correct values', async () => {
            stubCreate(BuildingCode)
            stubCreate(FloorNumber)
            stubCreate(Description)
            stubCreate(Floor)

            const buildingCode = 'B'
            const dto: IFloorDTO = {
                floorNumber: 2,
                buildingCode,
                description: 'test',
            }

            let buildingRepo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(buildingRepo, 'findByCode').resolves({
                code: 'B',
            } as unknown as Building)

            let floorRepo = Container.get('FloorRepo') as IFloorRepo
            sinon.stub(floorRepo, 'findByCodeNumber').resolves(null)
            sinon.stub(floorRepo, 'save').resolves('oi' as unknown as Floor)

            let passageRepo = Container.get('passageRepo') as IPassageRepo
            sinon.stub(FloorMap, 'toDTO').returns('works!' as unknown as IFloorDTO)

            const service = new FloorService(floorRepo, buildingRepo, passageRepo)
            const result = await service.createFloor(dto, buildingCode)

            let actual: Boolean
            actual = result.isRight()

            assert.isTrue(actual)
        })

        it('should fail if building is not found', async () => {
            stubCreate(BuildingCode)
            stubCreate(FloorNumber)
            stubCreate(Description)
            stubCreate(Floor)

            const buildingCode = 'B'
            const dto: IFloorDTO = {
                floorNumber: 2,
                buildingCode,
                description: 'test',
            }

            let buildingRepo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(buildingRepo, 'findByCode').resolves(null)

            let floorRepo = Container.get('FloorRepo') as IFloorRepo
            sinon.stub(floorRepo, 'findByCodeNumber').resolves(null)
            sinon.stub(floorRepo, 'save').resolves('oi' as unknown as Floor)

            let passageRepo = Container.get('passageRepo') as IPassageRepo
            sinon.stub(FloorMap, 'toDTO').returns('works!' as unknown as IFloorDTO)

            const service = new FloorService(floorRepo, buildingRepo, passageRepo)
            const result = await service.createFloor(dto, buildingCode)

            let actual: Boolean
            actual = result.isLeft()

            assert.isTrue(actual)
        })

        it('should fail if floor already exists', async () => {
            stubCreate(BuildingCode)
            stubCreate(FloorNumber)
            stubCreate(Description)
            stubCreate(Floor)

            const buildingCode = 'B'
            const dto: IFloorDTO = {
                floorNumber: 2,
                buildingCode,
                description: 'test',
            }

            let buildingRepo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(buildingRepo, 'findByCode').resolves({
                code: 'B',
            } as unknown as Building)

            let floorRepo = Container.get('FloorRepo') as IFloorRepo
            sinon.stub(floorRepo, 'findByCodeNumber').resolves({
                floorNumber: 3,
            } as unknown as Floor)
            sinon.stub(floorRepo, 'save').resolves('oi' as unknown as Floor)

            let passageRepo = Container.get('passageRepo') as IPassageRepo
            sinon.stub(FloorMap, 'toDTO').returns('works!' as unknown as IFloorDTO)

            const service = new FloorService(floorRepo, buildingRepo, passageRepo)
            const result = await service.createFloor(dto, buildingCode)

            let actual: Boolean
            actual = result.isLeft()

            assert.isTrue(actual)
        })
    })
    describe('patchFloor()', () => {
        //     it('should work with correct values', async () => {
        //         stubCreate(BuildingCode)
        //         stubCreate(FloorNumber)
        //         stubCreate(Description)
        //         stubCreate(Floor)
        //
        //         const dto: IUpdateFloorDTO = {
        //             buildingCode: 'B',
        //             oldFloorNumber: 1,
        //             floorNumber: 2,
        //         }
        //
        //         let buildingRepo = Container.get('BuildingRepo') as IBuildingRepo
        //         sinon.stub(buildingRepo, 'findByCode').resolves({
        //             code: 'B',
        //         } as unknown as Building)
        //
        //         let floorRepo = Container.get('FloorRepo') as IFloorRepo
        //         sinon.stub(floorRepo, 'findByCodeNumber').resolves({
        //             floorNumber: 1,
        //         } as unknown as Floor)
        //         sinon.stub(floorRepo, 'save').resolves('oi' as unknown as Floor)
        //
        //         let passageRepo = Container.get('passageRepo') as IPassageRepo
        //         sinon.stub(FloorMap, 'toDTO').returns('works!' as unknown as IFloorDTO)
        //
        //         const service = new FloorService(floorRepo, buildingRepo, passageRepo)
        //         const result = await service.patchFloor(dto)
        //
        //         let actual: Boolean
        //         actual = result.isRight()
        //
        //         assert.isTrue(actual)
        //     })

        it('should fail if building is not found', async () => {
            stubCreate(BuildingCode)
            stubCreate(FloorNumber)
            stubCreate(Description)
            stubCreate(Floor)

            const dto: IUpdateFloorDTO = {
                buildingCode: 'B',
                oldFloorNumber: 1,
                floorNumber: 2,
            }

            let buildingRepo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(buildingRepo, 'findByCode').resolves(null)

            let floorRepo = Container.get('FloorRepo') as IFloorRepo
            sinon.stub(floorRepo, 'findByCodeNumber').resolves({
                floorNumber: 1,
            } as unknown as Floor)
            sinon.stub(floorRepo, 'save').resolves('oi' as unknown as Floor)

            let passageRepo = Container.get('passageRepo') as IPassageRepo
            sinon.stub(FloorMap, 'toDTO').returns('works!' as unknown as IFloorDTO)

            const service = new FloorService(floorRepo, buildingRepo, passageRepo)
            const result = await service.patchFloor(dto)

            let actual: Boolean
            actual = result.isLeft()

            assert.isTrue(actual)
        })

        it('should fail if floor does not exist', async () => {
            stubCreate(BuildingCode)
            stubCreate(FloorNumber)
            stubCreate(Description)
            stubCreate(Floor)

            const dto: IUpdateFloorDTO = {
                buildingCode: 'B',
                oldFloorNumber: 1,
                floorNumber: 2,
            }

            let buildingRepo = Container.get('BuildingRepo') as IBuildingRepo
            sinon.stub(buildingRepo, 'findByCode').resolves({
                code: 'B',
            } as unknown as Building)

            let floorRepo = Container.get('FloorRepo') as IFloorRepo
            sinon.stub(floorRepo, 'findByCodeNumber').resolves(null)
            sinon.stub(floorRepo, 'save').resolves('oi' as unknown as Floor)

            let passageRepo = Container.get('passageRepo') as IPassageRepo
            sinon.stub(FloorMap, 'toDTO').returns('works!' as unknown as IFloorDTO)

            const service = new FloorService(floorRepo, buildingRepo, passageRepo)
            const result = await service.patchFloor(dto)

            let actual: Boolean
            actual = result.isLeft()

            assert.isTrue(actual)
        })
    })
})
