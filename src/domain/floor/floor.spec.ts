import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'

import { Result } from '../../core/logic/Result'

import Building, { BuildingProps } from '../building/building'


import { Floor } from '../floor/floor'
import { BuildingDescription as Description } from '../building/description'
import { FloorNumber as Number } from '../floor/floorNumber'

describe('Floor', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    let building: Building, description: Description, floorNumber: Number

    beforeEach(() => {
        stubCreate(Building)

        building = Building.create({} as BuildingProps).getValue()
        description = Description.create('2rd Floor').getValue()
        floorNumber = Number.create(3).getValue()
    })

    afterEach(sinon.restore)

    it('test for building undefined ', () => {
        const result = Floor.create({
            building: undefined,
            floorNumber,
            description,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('test for floorNumber undefined ', () => {
        const result = Floor.create({
            building,
            floorNumber: undefined,
            description,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('test for description undefined ', () => {
        const result = Floor.create({
            building,
            floorNumber,
            description: undefined,
        })

        assert.isNotOk(result.isSuccess)
    })

    it('test for floorNumber undefined ', () => {
        const result = Floor.create({
            building,
            floorNumber,
            description,
        })

        assert.isOk(result.isSuccess)
    })
})
