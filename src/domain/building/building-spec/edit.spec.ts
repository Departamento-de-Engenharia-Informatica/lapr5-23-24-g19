import { expect } from 'chai'
import { afterEach } from 'mocha'
import { createSandbox } from 'sinon'
import { Result } from '../../../core/logic/Result'
import Building from '../building'
import { BuildingCode } from '../code'
import { BuildingDescription } from '../description'
import { MaxFloorDimensions } from '../maxFloorDimensions'
import { BuildingName } from '../name'

describe('Building edit', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    let building: Building

    beforeEach(() => {
        stubCreate(BuildingCode)
        stubCreate(BuildingName)
        stubCreate(BuildingDescription)
        stubCreate(MaxFloorDimensions)

        const code = BuildingCode.create('B001').getValue()
        const name = BuildingName.create('Test Building').getValue()
        const description = BuildingDescription.create('A test building').getValue()
        const maxFloorDimensions = MaxFloorDimensions.create(10, 20).getValue()

        building = Building.create({
            code,
            name,
            description,
            maxFloorDimensions,
        }).getValue()
    })

    afterEach(() => sinon.restore())

    it('should allow changing the name', () => {
        const newName = BuildingName.create('New name').getValue()

        building.name = newName

        expect(building.name).to.equal(newName)
    })

    it('should allow changing the description', () => {
        const desc = BuildingDescription.create('New Desc').getValue()

        building.description = desc

        expect(building.description).to.equal(desc)
    })

    it('should allow changing the dimensions', () => {
        const dims = MaxFloorDimensions.create(28, 29).getValue()

        building.maxFloorDimensions = dims
        expect(building.maxFloorDimensions).to.equal(dims)
    })
})
