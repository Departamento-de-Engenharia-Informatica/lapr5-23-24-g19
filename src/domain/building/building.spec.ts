import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'

import { Result } from '../../core/logic/Result'

import Building, { BuildingProps } from '../building/building'

//import {Building,  BuildingProps } from '../building/building'
//import { BuildingId } from './buildingId'
import { BuildingName as Name } from './buildingName'
import { BuildingCode as Code } from './buildingCode'
import { BuildingDescription as Description } from './description'
import { MaxFloorDimensions } from './maxFloorDimensions'

describe('Building', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    let code: Code, name: Name, description: Description, maxFloorDimensions: MaxFloorDimensions
    let building: Building

    beforeEach(() => {
        stubCreate(Building)

        building = Building.create({} as BuildingProps).getValue()

        code = Code.create('B001').getValue()
        name = Name.create('Test Building').getValue()
        description = Description.create('A test building').getValue()
        maxFloorDimensions = MaxFloorDimensions.create(10, 20).getValue()
    })

    afterEach(sinon.restore)

    it('allows changing the name, description, and max floor dimensions', () => {
        const updatedName = Name.create('Updated Building Name').getValue()
        const updatedDescription = Description.create('An updated building').getValue()
        const updatedDimensions = MaxFloorDimensions.create(10, 20).getValue()

        const building = Building.create({
            code,
            name,
            description,
            maxFloorDimensions,
        }).getValue()

        building.name = updatedName
        assert.strictEqual(building.name, updatedName)

        building.description = updatedDescription
        assert.strictEqual(building.description, updatedDescription)

        building.maxFloorDimensions = updatedDimensions
        assert.deepStrictEqual(building.maxFloorDimensions, updatedDimensions)
    })
})
