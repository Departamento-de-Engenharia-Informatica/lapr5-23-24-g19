import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'

import Building from '../building'
import { BuildingName } from '../name'
import { BuildingCode } from '../code'
import { BuildingDescription } from '../description'
import { MaxFloorDimensions } from '../maxFloorDimensions'
import { Result } from '../../../core/logic/Result'

describe('Building create', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    afterEach(sinon.restore)

    it('should fail when creating a building with null or undefined code', () => {
        stubCreate(BuildingName)
        stubCreate(BuildingDescription)
        stubCreate(MaxFloorDimensions)

        const result = Building.create({
            code: undefined,
            name: BuildingName.create('').getValue(),
            description: BuildingDescription.create('').getValue(),
            maxFloorDimensions: MaxFloorDimensions.create(0, 0).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })
    it('accepts no name', () => {
        stubCreate(BuildingCode)
        stubCreate(BuildingDescription)
        stubCreate(MaxFloorDimensions)

        const result = Building.create({
            code: BuildingCode.create('').getValue(),
            name: undefined,
            description: BuildingDescription.create('').getValue(),
            maxFloorDimensions: MaxFloorDimensions.create(0, 0).getValue(),
        })

        assert.isOk(result.isSuccess)
    })

    it('accepts no description', () => {
        stubCreate(BuildingCode)
        stubCreate(BuildingName)
        stubCreate(MaxFloorDimensions)
        stubCreate(BuildingDescription)

        let result = Building.create({
            code: BuildingCode.create('').getValue(),
            name: BuildingName.create('').getValue(),
            maxFloorDimensions: MaxFloorDimensions.create(0, 0).getValue(),

            description: undefined,
        })

        assert.isOk(result.isSuccess)

        result = Building.create({
            code: BuildingCode.create('').getValue(),
            name: BuildingName.create('').getValue(),
            maxFloorDimensions: MaxFloorDimensions.create(0, 0).getValue(),

            description: BuildingDescription.create('Test').getValue(),
        })

        assert.isOk(result.isSuccess)
    })

    it('requires maxFloorDimensions', () => {
        stubCreate(BuildingCode)
        stubCreate(BuildingName)
        stubCreate(BuildingDescription)
        stubCreate(MaxFloorDimensions)

        let result = Building.create({
            code: BuildingCode.create('').getValue(),
            name: BuildingName.create('').getValue(),
            description: BuildingDescription.create('').getValue(),
            maxFloorDimensions: undefined,
        })

        assert.isNotOk(result.isSuccess)

        result = Building.create({
            code: BuildingCode.create('').getValue(),
            name: BuildingName.create('').getValue(),
            description: BuildingDescription.create('').getValue(),
            maxFloorDimensions: MaxFloorDimensions.create(0, 0).getValue(),
        })

        assert.isOk(result.isSuccess)
    })
})
