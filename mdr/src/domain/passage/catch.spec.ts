import { assert, expect } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'

import { Result } from '../../core/logic/Result'

import Building from '../building/building'

import { FloorNumber, FloorNumber as Number } from '../floor/floorNumber'
import { BuildingCode } from '../building/code'
import { MaxFloorDimensions } from '../building/maxFloorDimensions'
import { Floor } from '../floor/floor'
import { Passage } from './passage'

describe('Passage integration', () => {
    const sinon = createSandbox()
    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    let code1: BuildingCode,
        code2: BuildingCode,
        maxFloorDimensionsRes: MaxFloorDimensions
    let building1: Building, building2: Building

    beforeEach(() => {
        stubCreate(BuildingCode)
        stubCreate(FloorNumber)
        sinon
            .stub(MaxFloorDimensions, 'create')
            .returns(Result.ok({ width: 10, length: 10 } as MaxFloorDimensions))
        maxFloorDimensionsRes = MaxFloorDimensions.create(30, 30).getValue()

        code1 = BuildingCode.create('').getValue()
        building1 = Building.create({
            code: code1,
            maxFloorDimensions: maxFloorDimensionsRes,
        }).getValue()

        code2 = BuildingCode.create('').getValue()
        building2 = Building.create({
            code: code2,
            maxFloorDimensions: maxFloorDimensionsRes,
        }).getValue()
    })

    afterEach(sinon.restore)

    it('should add passage, different building', () => {
        const floor1 = Floor.create({
            building: building1,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        const floor2 = Floor.create({
            building: building2,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        const passage = Passage.create({
            floor1: floor1,
            floor2: floor2,
        })

        assert.isOk(passage.isSuccess)
    })
    it('should not add passage, same building', () => {
        const floor1 = Floor.create({
            building: building1,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        const floor2 = Floor.create({
            building: building1,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        const passage = Passage.create({
            floor1: floor1,
            floor2: floor2,
        })

        assert.isNotOk(passage.isSuccess)
    })
    it('should update passage, different building', () => {
        const floor1 = Floor.create({
            building: building1,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        const floor2 = Floor.create({
            building: building2,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        const passage = Passage.create({
            floor1: floor1,
            floor2: floor2,
        })
        assert.isOk(passage.isSuccess)

        const code3 = BuildingCode.create('').getValue()
        const building3 = Building.create({
            code: code3,
            maxFloorDimensions: maxFloorDimensionsRes,
        }).getValue()
        const floor3 = Floor.create({
            building: building3,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        assert.isOk(passage.getValue().update({ floor1: floor3 }).isSuccess)
    })
    it('should not update passage, same building', () => {
        const floor1 = Floor.create({
            building: building1,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        const floor2 = Floor.create({
            building: building2,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        const passage = Passage.create({
            floor1: floor1,
            floor2: floor2,
        })
        assert.isOk(passage.isSuccess)

        const floor3 = Floor.create({
            building: building2,
            floorNumber: FloorNumber.create(1).getValue(),
        }).getValue()

        assert.isNotOk(passage.getValue().update({ floor1: floor3 }).isSuccess)
    })
})
