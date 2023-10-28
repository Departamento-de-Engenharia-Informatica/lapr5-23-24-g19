import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'
import Building from './building'
import { BuildingName } from './buildingName'
import { BuildingCode } from './buildingCode'
import { BuildingDescription } from './description'
import { MaxFloorDimensions } from './maxFloorDimensions'
import { Result } from '../../core/logic/Result'

describe('Building', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    beforeEach(() => {})

    afterEach(sinon.restore)

    it('should allow changing the name, description, and max floor dimensions', () => {
        const code = BuildingCode.create('B001').getValue()
        const name = BuildingName.create('Test Building').getValue()
        const description = BuildingDescription.create('A test building').getValue()
        const maxFloorDimensions = MaxFloorDimensions.create(10, 20).getValue()

        const result = Building.create({
            code,
            name,
            description,
            maxFloorDimensions,
        })

        const updatedName = BuildingName.create('Updated Building Name').getValue()
        const updatedDescription = BuildingDescription.create('An updated building').getValue()
        const updatedDimensions = MaxFloorDimensions.create(10, 20).getValue()

        result.getValue().name = updatedName
        assert.isOk(result.isSuccess)

        result.getValue().description = updatedDescription
        assert.isOk(result.isSuccess)

        result.getValue().maxFloorDimensions = updatedDimensions
        assert.isOk(result.isSuccess)
    })

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

    it('should fail when creating a building with null or undefined name', () => {
        stubCreate(BuildingCode)
        stubCreate(BuildingDescription)
        stubCreate(MaxFloorDimensions)

        const result = Building.create({
            code: BuildingCode.create('').getValue(),
            name: undefined,
            description: BuildingDescription.create('').getValue(),
            maxFloorDimensions: MaxFloorDimensions.create(0, 0).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a building with null or undefined description', () => {
        stubCreate(BuildingCode)
        stubCreate(BuildingName)
        stubCreate(MaxFloorDimensions)

        const result = Building.create({
            code: BuildingCode.create('').getValue(),
            name: BuildingName.create('').getValue(),
            description: undefined,
            maxFloorDimensions: MaxFloorDimensions.create(0, 0).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a building with null or undefined max floor dimensions', () => {
        stubCreate(BuildingCode)
        stubCreate(BuildingName)
        stubCreate(BuildingDescription)

        const result = Building.create({
            code: BuildingCode.create('').getValue(),
            name: BuildingName.create('').getValue(),
            description: BuildingDescription.create('').getValue(),
            maxFloorDimensions: undefined,
        })

        assert.isNotOk(result.isSuccess)
    })
})
