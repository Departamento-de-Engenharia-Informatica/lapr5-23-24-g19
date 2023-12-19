import { assert, expect } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'

import { Result } from '../../core/logic/Result'

import Building, { BuildingProps } from '../building/building'

import { Floor } from '../floor/floor'
import { BuildingDescription as Description } from '../building/description'
import { FloorNumber, FloorNumber as Number } from '../floor/floorNumber'

describe('Floor', () => {
    const sinon = createSandbox()
    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    let building: Building, description: Description, floorNumber: Number

    beforeEach(() => {
        stubCreate(Building)
        stubCreate(Description)
        stubCreate(Number)

        building = Building.create({} as BuildingProps).getValue()
        description = Description.create('asdas').getValue()
        floorNumber = Number.create(3).getValue()
    })

    afterEach(sinon.restore)

    it('should fail for building undefined ', () => {
        const result = Floor.create({
            building: undefined,
            floorNumber,
            description,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail for floorNumber undefined ', () => {
        const result = Floor.create({
            building,
            floorNumber: undefined,
            description,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should work for description undefined ', () => {
        const result = Floor.create({
            building,
            floorNumber,
            description: undefined,
        })

        assert.isOk(result.isSuccess)
    })

    it('should work for map undefined ', () => {
        const result = Floor.create({
            building,
            floorNumber,
            path: undefined,
        })

        assert.isOk(result.isSuccess)
    })

    it('should work for none undefined ', () => {
        const result = Floor.create({
            building,
            floorNumber,
            description,
        })

        assert.isOk(result.isSuccess)
    })

    it('should change building', () => {
        const result = Floor.create({
            building,
            floorNumber,
            description,
        })

        assert.isOk(result.isSuccess)
    })

    it('should change floorNumber', () => {
        const result = Floor.create({
            building,
            floorNumber,
            description,
        })
        assert.isOk(result.isSuccess)
        const newFloor = FloorNumber.create(1)
        assert.isOk(newFloor.isSuccess)
        result.getValue().floorNumber = newFloor.getValue()
        expect(result.getValue().floorNumber.value).to.equal(floorNumber.value)
    })

    it('should change Description', () => {
        const result = Floor.create({
            building,
            floorNumber,
            description,
        })
        assert.isOk(result.isSuccess)
        const newDescription = Description.create('des')
        assert.isOk(newDescription)
        result.getValue().description = newDescription.getValue()
        expect(result.getValue().description.value).to.equal(description.value)
    })
    it('return same building', () => {
        const result = Floor.create({
            building,
            floorNumber,
            description,
        })
        assert.isOk(result.isSuccess)
        expect(result.getValue().building).to.equal(building)
    })
})
